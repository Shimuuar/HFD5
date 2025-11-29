{-# LANGUAGE OverloadedStrings #-}
-- |
-- Description of dataspaces.
module HDF5.HL.Internal.Dataspace
  ( -- * Concrete type
--    Extent(..)
    -- * Encoding of extents
    IsExtent(..)
  , IsDataspace(..)
  , pattern UNLIMITED
  , ParserDim
  , parseDim
  , endOfExtent
  , runParseFromDataspace
    -- * Creation of dataspaces
  , createDataspaceFromExtent
  , createDataspaceFromDSpace
  , withCreateDataspaceFromExtent
  , withCreateDataspaceFromDSpace
  , setSlabSelection
    -- * Internal
  , withEncodedExtent
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
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import GHC.Stack

import HDF5.HL.Internal.Wrappers
import HDF5.HL.Internal.Error
import HDF5.C


----------------------------------------------------------------
-- Haskell data type
----------------------------------------------------------------

-- -- | Generic extent of dataspace. It could encode all possible
-- --   extents.
-- data Extent
--   = Simple [Word64] -- ^ Simple dataspace.
--   | Null            -- ^ Null extent for dataset which do not contain any actual data.
--   deriving stock (Show,Eq,Ord)


-- | Type class for values representing some product of @Word64@ indexes.
--   It could be number for 1D arrays or some combination of tuples.
class IsExtent a where
  -- | Encode extent. This fold over all dimensions of dataset for
  --   simple and scalar extents. Null extents should return @Nothing@.
  encodeExtent :: Monoid m => a -> (Word64 -> m) -> m


-- | Type class for values representing dataspace. It could be either
--   null which is used for datasets containing no data, or
--   N-dimensional size and possibly maximal size.
class IsDataspace a where
  encodeDataspace :: Monoid m => a -> Maybe ((Word64 -> Word64 -> m) -> m)
  -- | Parser for dataset which could be used to decode from sequence
  --   of Dims.
  decodeDataspace :: Monad m => ParserDim (Word64,Word64) m a
  decodeNullDataspace :: Maybe a
  decodeNullDataspace = Nothing


-- FIXME: Decide what to do with overflows???

-- | Extent for scalar values. It's rank-0 extent not null extent!
instance IsExtent () where
  encodeExtent ()  = \_ -> mempty

-- instance IsExtent Extent where
--   encodeExtent Null          = Nothing
--   encodeExtent (Simple dims) = Just $ flip foldMap dims
--   decodeExtent     = Simple <$> many parseDim
--   -- decodeNullExtent = Just Null

instance IsExtent Word64 where
  encodeExtent i = \f -> f i
instance IsExtent Word where
  encodeExtent i = \f -> f (fromIntegral i)
instance IsExtent Int64 where
  encodeExtent i = \f -> f (if i < 0 then error "Negative extent" else fromIntegral i)
instance IsExtent Int where
  encodeExtent i = \f -> f (if i < 0 then error "Negative extent" else fromIntegral i)

instance (IsExtent a, IsExtent b) => IsExtent (a,b) where
  encodeExtent (a,b) f = encodeExtent a f <> encodeExtent b f

instance IsExtent a => IsExtent [a] where
  encodeExtent xs f = foldMap (\x -> encodeExtent x f) xs



instance IsDataspace () where
  encodeDataspace () = Just $ \_ -> mempty
  decodeDataspace    = pure ()

instance IsDataspace Word64 where
  encodeDataspace i = Just $ \f ->f i i
  decodeDataspace = fst <$> parseDim

instance IsDataspace Word where
  encodeDataspace (fromIntegral -> i) = Just $ \f ->f i i
  decodeDataspace = fromIntegral <$> decodeDataspace @Word64

-- FIXME: Int,Int64

instance (IsDataspace a, IsDataspace b) => IsDataspace (a,b) where
  encodeDataspace (a,b) = (liftA2 . liftA2) (<>)
    (encodeDataspace a) (encodeDataspace b)
  decodeDataspace = liftA2 (,) decodeDataspace decodeDataspace

instance (IsDataspace a) => IsDataspace [a] where
  encodeDataspace xs = do
    fs <- traverse encodeDataspace xs
    Just $ \f -> foldMap ($ f) fs
  decodeDataspace = many decodeDataspace



-- | Value which is used to represent than maximum size of particular
--   dimension is unbounded.
pattern UNLIMITED :: Word64
pattern UNLIMITED <- (coerce -> H5S_UNLIMITED) where UNLIMITED = coerce H5S_UNLIMITED



----------------------------------------------------------------
-- Parser for dataspaces
----------------------------------------------------------------

-- | Very simple parser for sequence of values of type @i@
newtype ParserDim i m a = ParserDim
  { _unParserDim :: forall s.
                    (s -> m (Maybe (s, i)))
                 -> (s -> m (Maybe (s, a)))
  }
  deriving stock Functor

instance Monad m => Applicative (ParserDim i m) where
  pure a = ParserDim $ \_ s -> pure (Just (s,a))
  ParserDim pf <*> ParserDim pa = ParserDim $ \uncons s -> runMaybeT $ do
    (s',  f) <- MaybeT $ pf uncons s
    (s'', a) <- MaybeT $ pa uncons s'
    pure (s'', f a)

instance Monad m => Alternative (ParserDim i m) where
  empty = ParserDim $ \_ _ -> pure Nothing
  ParserDim pa <|> ParserDim pb = ParserDim $ \uncons s -> runMaybeT (MaybeT (pa uncons s) <|> MaybeT (pb uncons s))

parseDim :: ParserDim i m i
parseDim = ParserDim id

endOfExtent :: Monad m => ParserDim i m ()
endOfExtent = ParserDim $ \uncons s -> uncons s >>= \case
  Nothing -> pure $ Just (s,())
  Just _  -> pure Nothing

runParserDim
  :: Monad m
  => (s -> m (Maybe (s,i)))
  -> s
  -> ParserDim i m a
  -> m (Maybe a)
runParserDim uncons s0 (ParserDim fun) = fmap snd <$> fun uncons s0


runParseFromDataspace :: IsDataspace a => Dataspace -> IO (Maybe a)
runParseFromDataspace (getHID -> hid) = withFrozenCallStack $ evalContT $ do
  p_err <- ContT alloca
  lift (h5s_get_simple_extent_type hid p_err) >>= \case
    H5S_NULL   -> pure $ decodeNullDataspace
    H5S_SCALAR -> runParserDim (\() -> pure Nothing) ()  decodeDataspace
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
        let uncons i | i >= rank = pure Nothing
                     | otherwise = do
                         dim  <- peekElemOff p_dim i
                         mdim <- peekElemOff p_max i
                         pure $ Just (i+1, (dim, mdim))
        runParserDim uncons 0  decodeDataspace
    _ -> lift $ throwM =<< decodeError p_err "Cannot get class of dataspace"



----------------------------------------------------------------
-- Encoder for dataspaces
----------------------------------------------------------------

-- | Monoid which is used to fill buffers for calling HDF5 functions.
newtype DSpaceWriter = DSpaceWriter
  (  Ptr HSize -- Pointer to size
  -> Int       -- Maximum offset
  -> Int       -- Offset
  -> IO Int
  )

instance Semigroup DSpaceWriter where
  {-# INLINE (<>) #-}
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

putDimension2 :: Word64 -> Word64 -> DSpaceWriter
putDimension2 sz max_sz = DSpaceWriter go where
  go p_sz i_max i
    | i >= i_max = throwM $ Error [Left "Internal error: buffer overrun"]
    | otherwise  = do pokeElemOff p_sz i             (coerce sz)
                      pokeElemOff p_sz (maxRank + i) (coerce max_sz)
                      pure $! i + 1


-- Maximum possible rank of an array (as of HDF5 1.8)
maxRank :: Int
maxRank = 32


----------------------------------------------------------------
-- Dataspace creation
----------------------------------------------------------------

withEncodedExtent
  :: (IsExtent dim)
  => dim -> ContT r IO (Int, Ptr HSize)
withEncodedExtent dim = do
  let DSpaceWriter write = encodeExtent dim putDimension
  ptr  <- ContT $ allocaArray maxRank
  rank <- lift  $ write ptr maxRank 0
  pure (rank, ptr)

withEncodedDataspace
  :: (IsDataspace dim)
  => dim -> ContT r IO (Maybe (Int, Ptr HSize, Ptr HSize))
withEncodedDataspace dim =
  case encodeDataspace dim of
    Nothing  -> pure Nothing
    Just enc -> do
      let DSpaceWriter write = enc putDimension2
      ptr  <- ContT $ allocaArray (maxRank * 2)
      rank <- lift  $ write ptr maxRank 0
      pure $ Just ( rank
                  , ptr
                  , ptr `plusPtr` (maxRank * sizeOf (undefined :: HSize)))




-- | Create dataspace for a given extent. This only creates scalar or
--   simple dataspaces with maximum size same as real size.
createDataspaceFromExtent
  :: (IsExtent dim, HasCallStack)
  => dim       -- ^ Extent of dataspace
  -> IO Dataspace
createDataspaceFromExtent dim = withFrozenCallStack $ evalContT $ do
  p_err      <- ContT $ alloca
  (rank,ptr) <- withEncodedExtent dim
  lift $ fmap Dataspace
       $ checkHID p_err "Unable to create simple dataspace"
       $ h5s_create_simple (fromIntegral rank) ptr nullPtr


-- | Create dataspace for a given extent. This variant allow creation
--   of all possible dataspaces.
createDataspaceFromDSpace
  :: (IsDataspace dim, HasCallStack)
  => dim       -- ^ Extent of dataspace
  -> IO Dataspace
createDataspaceFromDSpace dim = withFrozenCallStack $ evalContT $ do
  p_err <- ContT $ alloca
  withEncodedDataspace dim >>= \case
    Nothing  -> lift $ fmap Dataspace
                     $ checkHID p_err "Unable to create dataspace with NULL extent"
                     $ h5s_create H5S_NULL
    Just (rank,p_sz,p_max) -> do
      lift $ fmap Dataspace
           $ checkHID p_err "Unable to create simple dataspace"
           $ h5s_create_simple (fromIntegral rank) p_sz p_max


-- | Bracket wrapping 'createDataspaceFromExtent'
withCreateDataspaceFromExtent
  :: IsExtent dim
  => dim                 -- ^ Extent of dataspace
  -> (Dataspace -> IO a) -- ^ Continuation
  -> IO a
withCreateDataspaceFromExtent dim
  = bracket (createDataspaceFromExtent dim) basicClose

-- | Bracket wrapping 'createDataspaceFromDSpace'
withCreateDataspaceFromDSpace
  :: IsDataspace dim
  => dim                 -- ^ Extent of dataspace
  -> (Dataspace -> IO a) -- ^ Continuation
  -> IO a
withCreateDataspaceFromDSpace dim
  = bracket (createDataspaceFromDSpace dim) basicClose


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
  (rank_off, p_off) <- withEncodedExtent off
  (rank_sz , p_sz)  <- withEncodedExtent sz
  when (rank_off /= rank_sz) $ throwM $
    Error [Left "In dataspace selection ranks of an offset and size do not match"]
  when (fromIntegral rank_dset /= rank_sz) $ throwM $
    Error [Left "Rank of size does not match rank of dataset"]
  lift $ checkHErr p_err "Unable to set simple hyperslab selection"
       $ h5s_select_hyperslab hid H5S_SELECT_SET
            p_off nullPtr
            p_sz  nullPtr
  pure ()
