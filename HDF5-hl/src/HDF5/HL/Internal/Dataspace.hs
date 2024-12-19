{-# LANGUAGE OverloadedStrings #-}
-- |
-- Description of dataspaces.
module HDF5.HL.Internal.Dataspace
  ( -- * Concrete type
    Extent(..)
    -- * Encoding of extents
  , IsExtent(..)
  , pattern UNLIMITED
  , ParserDim
  , parseDim
  , endOfExtent
  , runParseFromDataspace
    -- * Helpers for working with datasets
  , DSpaceWriter
  , putDimension
    -- * Creation of dataspaces
  , createDataspace
  , withCreateDataspace
  , withEncodedExtent
  , setSlabSelection
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Int
import Data.Word
import Data.Functor
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.Types
import GHC.Stack

import HDF5.HL.Internal.Wrappers
import HDF5.HL.Internal.Error
import HDF5.C


----------------------------------------------------------------
-- Haskell data type
----------------------------------------------------------------

-- | Generic extent of dataspace. It could encode all possible
--   extents.
data Extent
  = Simple [Word64] -- ^ Simple dataspace.
  | Null            -- ^ Null extent for dataset which do not contain any actual data.
  deriving stock (Show,Eq,Ord)


-- | Type class to values which represent subset of possible
--   extents. One usually wants to work with more concrete types such
--   as one or two dimensional array. In that case working with
--   'Extent' would be inconvenient
class IsExtent a where
  -- | Encode extent. This fold over all dimensions of dataset for
  --   simple and scalar extents. Null extents should return @Nothing@.
  encodeExtent :: Monoid m => a -> Maybe ((Word64 -> m) -> m)
  -- | Parser for dataset which could be used to decode from sequence
  --   of Dims.
  decodeExtent :: Monad m => ParserDim m a
  -- | How null extent should be decoded
  decodeNullExtent :: Maybe a
  decodeNullExtent = Nothing


newtype ParserDim m a = ParserDim
  { unParserDim :: forall s.
                   (s -> m (Maybe (s, Word64))) -- Uncons (possibly monadic)
                -> (s -> m (Maybe (s, a)))      -- State monad with failure
  }
  deriving Functor

instance Monad m => Applicative (ParserDim m) where
  pure a = ParserDim $ \_ s -> pure (Just (s,a))
  ParserDim pf <*> ParserDim pa = ParserDim $ \uncons s -> runMaybeT $ do
    (s',  f) <- MaybeT $ pf uncons s
    (s'', a) <- MaybeT $ pa uncons s'
    pure (s'', f a)

instance Monad m => Alternative (ParserDim m) where
  empty = ParserDim $ \_ _ -> pure Nothing
  ParserDim pa <|> ParserDim pb = ParserDim $ \uncons s -> runMaybeT (MaybeT (pa uncons s) <|> MaybeT (pb uncons s))

parseDim :: ParserDim m Word64
parseDim = ParserDim id

endOfExtent :: Monad m => ParserDim m ()
endOfExtent = ParserDim $ \uncons s -> uncons s >>= \case
  Nothing -> pure $ Just (s,())
  Just _  -> pure Nothing

runParseFromDataspace :: IsExtent a => Dataspace -> IO (Maybe (a,a))
runParseFromDataspace (getHID -> hid) = withFrozenCallStack $ evalContT $ do
  p_err <- ContT alloca
  lift (h5s_get_simple_extent_type hid p_err) >>= \case
    H5S_NULL   -> pure $ decodeNullExtent
    H5S_SCALAR -> unParserDim decodeExtent (\() -> pure Nothing) () <&> fmap snd
    H5S_SIMPLE -> do
      rank <- lift
            $ fmap fromIntegral
            $ checkCInt p_err "Cannot get rank of simple extent"
            $ h5s_get_simple_extent_ndims hid
      -- Allocate buffers
      p_dim <- ContT $ allocaArray rank
      p_max <- ContT $ allocaArray rank
      lift $ do
        _ <- checkCInt p_err "Cannot get extent for simple dataspace"
           $ h5s_get_simple_extent_dims hid p_dim p_max
        --
        let uncons p i | i >= rank = pure Nothing
                       | otherwise = do dim <- fromIntegral <$> peekElemOff p i
                                        pure $ Just (i+1, dim)
        dim     <- unParserDim decodeExtent (uncons p_dim) 0 <&> fmap snd
        dim_max <- unParserDim decodeExtent (uncons p_max) 0 <&> fmap snd
        pure $ liftA2 (,) dim dim_max
    _ -> lift $ throwM =<< decodeError p_err "Cannot get class of dataspace"

-- | Extent for scalar values. It's rank-0 extent not null extent!
instance IsExtent () where
  encodeExtent ()  = Just $ \_ -> mempty
  decodeExtent     = pure ()

instance IsExtent Extent where
  encodeExtent Null          = Nothing
  encodeExtent (Simple dims) = Just $ flip foldMap dims
  decodeExtent     = Simple <$> many parseDim
  decodeNullExtent = Just Null

instance IsExtent Word64 where
  encodeExtent i = Just $ \f -> f i
  decodeExtent   = parseDim

instance IsExtent Word where
  encodeExtent i = Just $ \f -> f (fromIntegral i)
  decodeExtent   = fromIntegral <$> parseDim

instance IsExtent Int64 where
  encodeExtent i = Just $ \f -> f (if i < 0 then error "Negative extent" else fromIntegral i)
  -- FIXME: Deal with overflows???
  decodeExtent   = fromIntegral <$> parseDim

instance IsExtent Int where
  encodeExtent i = Just $ \f -> f (if i < 0 then error "Negative extent" else fromIntegral i)
  -- FIXME: Deal with overflows???
  decodeExtent   = fromIntegral <$> parseDim

instance (IsExtent a, IsExtent b) => IsExtent (a,b) where
  encodeExtent (a,b) = do encA <- encodeExtent a
                          encB <- encodeExtent b
                          Just $ \f -> encA f <> encB f
  decodeExtent = (,) <$> decodeExtent <*> decodeExtent

pattern UNLIMITED :: Word64
pattern UNLIMITED <- (coerce -> H5S_UNLIMITED) where UNLIMITED = coerce H5S_UNLIMITED

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Monoid which is used to fill buffers for calling HDF5 functions.
newtype DSpaceWriter = DSpaceWriter
  (  Ptr HSize -- Pointer to size
  -> Int       -- Maximum offset
  -> Int       -- Offset
  -> IO Int
  )

instance Semigroup DSpaceWriter where
  DSpaceWriter f <> DSpaceWriter g = DSpaceWriter $ \p_sz i_max ->
    g p_sz i_max <=< f p_sz i_max

instance Monoid DSpaceWriter where
  {-# INLINE mempty #-}
  mempty = DSpaceWriter $ \_ _ -> pure


putDimension :: Word64 -> DSpaceWriter
putDimension sz = DSpaceWriter go where
  go p_sz i_max i
    | i >= i_max = throwM $ Error [Left "Internal error: buffer overrun"]
    | otherwise  = do pokeElemOff p_sz i (coerce sz)
                      pure $! i + 1


----------------------------------------------------------------
-- Dataspace creation
----------------------------------------------------------------

withEncodedExtent
  :: (IsExtent dim)
  => dim -> ContT r IO (Maybe (Int, Ptr HSize))
withEncodedExtent dim = case encodeExtent dim of
  Nothing  -> pure Nothing
  Just fld -> do let DSpaceWriter write = fld putDimension
                 ptr  <- ContT $ allocaArray max_rank
                 rank <- lift  $ write ptr max_rank 0
                 pure $ Just (rank, ptr)
  where
    -- We hardcode maximum rank at 32 (Which was the case in HDF5 1.8)
    max_rank = 32

-- | Create dataspace for a given extent
createDataspace
  :: (IsExtent dim, HasCallStack)
  => dim       -- ^ Extent of dataspace
  -> Maybe dim -- ^ Maximum extent of dataspace (optional)
  -> IO Dataspace
createDataspace dim dim_max = withFrozenCallStack $ evalContT $ do
  p_err <- ContT $ alloca
  -- Encode dataset extent
  withEncodedExtent dim >>= \case
    Nothing  -> lift $ fmap Dataspace
                     $ checkHID p_err "Unable to create dataspace with NULL extent"
                     $ h5s_create H5S_NULL
    Just (rank,ptr) -> do
      -- Encode optional max extent
      ptr_max <- case dim_max of
        Nothing -> pure nullPtr
        Just d  -> withEncodedExtent d >>= \case
          Nothing                -> throwM $ Error [Left "Maximum extent must have same rank"]
          Just (r,_) | r /= rank -> throwM $ Error [Left "Maximum extent must have same rank"]
          Just (_,p) -> pure p
      lift $ fmap Dataspace
           $ checkHID p_err "Unable to create simple dataspace"
           $ h5s_create_simple (fromIntegral rank) ptr ptr_max


-- | Create simple dataspace which could e used in bracket-like
--   context
withCreateDataspace
  :: IsExtent dim
  => dim                 -- ^ Extent of dataspace
  -> Maybe dim           -- ^ Maximum extent of dataspace
  -> (Dataspace -> IO a) -- ^ Continuation
  -> IO a
withCreateDataspace dim dim_max = bracket (createDataspace dim dim_max) basicClose

-- | Set selection in dataspace to a regular slab.
setSlabSelection
  :: (IsExtent dim)
  => Dataspace
  -> dim        -- ^ Offset
  -> dim        -- ^ Size of selection
  -> IO ()
setSlabSelection (Dataspace hid) off sz = evalContT $ do
  p_err     <- ContT alloca
  --
  rank_dset <- lift
             $ checkCInt p_err "Cannot get rank of dataspace's extent"
             $ h5s_get_simple_extent_ndims hid
  --
  (rank_off, p_off) <- checkJust =<< withEncodedExtent off
  (rank_sz , p_sz)  <- checkJust =<< withEncodedExtent sz
  when (rank_off /= rank_sz) $ throwM $
    Error [Left "In dataspace selection ranks of an offset and size do not match"]
  when (fromIntegral rank_dset /= rank_sz) $ throwM $
    Error [Left "Rank of size does not match rank of dataset"]
  lift $ checkHErr p_err "Unable to set simple hyperslab selection"
       $ h5s_select_hyperslab hid H5S_SELECT_SET
            p_off nullPtr
            p_sz  nullPtr
  pure ()
  where
    checkJust (Just a) = pure a
    checkJust  Nothing = throwM $ Error [Left "Selection must be non-null"]
