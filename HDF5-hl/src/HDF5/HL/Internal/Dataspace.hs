{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Description of dataspaces.
module HDF5.HL.Internal.Dataspace
  ( -- * Concrete type
    Dim(..)
  , Extent(..)
    -- * Encoding of extents
  , IsExtent(..)
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
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Int
import Data.Functor
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import GHC.Stack

import HDF5.HL.Internal.Types
import HDF5.HL.Internal.Error
import HDF5.C


----------------------------------------------------------------
-- Haskell data type
----------------------------------------------------------------

-- | Size of dimension of dataspace.
data Dim = Dim
  { dimSize    :: !Int64 -- ^ Current size 
  , dimMaxSize :: !Int64 -- ^ Maximum possible size
  }
  deriving stock (Show,Eq,Ord)

-- | Extent of dataspace.
data Extent
  = Simple [Dim] -- ^ Simple dataspace.
  | Null         -- ^ Null extent for dataset which do not contain any actual data.
  deriving stock (Show,Eq,Ord)


-- | Type class to values which represent subset of possible
--   extents. One usually wants to work with more concrete types such
--   as one or two dimensional array. In that case working with
--   'Extent' would be inconvenient
class IsExtent a where
  -- | Encode extent. This fold over all dimensions of dataset for
  --   simple and scalar extents. Null extents should return @Nothing@.
  encodeExtent :: Monoid m => a -> Maybe ((Dim -> m) -> m)
  -- | Parser for dataset which could be used to decode from sequence
  --   of Dims.
  decodeExtent :: Monad m => ParserDim m a
  -- | How null extent should be decoded
  decodeNullExtent :: Maybe a
  decodeNullExtent = Nothing


newtype ParserDim m a = ParserDim
  { unParserDim :: forall s.
                   (s -> m (Maybe (s, Dim))) -- Uncons (possibly monadic)
                -> (s -> m (Maybe (s, a)))   -- State monad with failure
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

parseDim :: ParserDim m Dim
parseDim = ParserDim id

endOfExtent :: Monad m => ParserDim m ()
endOfExtent = ParserDim $ \uncons s -> uncons s >>= \case
  Nothing -> pure $ Just (s,())
  Just _  -> pure Nothing

runParseFromDataspace :: IsExtent a => Dataspace -> IO (Maybe a)
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
        _ <- checkCInt p_err "Cannot get rank of simple extent"
           $ h5s_get_simple_extent_dims hid p_dim p_max
        --
        let uncons i | i >= rank = pure Nothing
                     | otherwise = do dim <- Dim <$> (fromIntegral <$> peekElemOff p_dim i)
                                                 <*> (fromIntegral <$> peekElemOff p_max i)
                                      pure $ Just (i+1, dim)
        unParserDim decodeExtent uncons 0 <&> fmap snd
    _ -> lift $ throwM =<< decodeError p_err "Cannot get class of dataspace"



instance IsExtent () where
  encodeExtent ()  = Just $ \_ -> mempty
  decodeExtent     = pure ()


instance IsExtent Extent where
  encodeExtent Null          = Nothing
  encodeExtent (Simple dims) = Just $ flip foldMap dims
  decodeExtent     = Simple <$> many parseDim
  decodeNullExtent = Just Null

instance IsExtent Dim where
  encodeExtent d = Just $ \f -> f d
  decodeExtent   = parseDim

instance IsExtent Int64 where
  encodeExtent i = Just $ \f -> f (Dim i i)
  decodeExtent   = dimSize <$> parseDim

instance IsExtent Int where
  encodeExtent i = Just $ \f -> let i' = fromIntegral i in f (Dim i' i')
  decodeExtent   = fromIntegral . dimSize <$> parseDim

instance (IsExtent a, IsExtent b) => IsExtent (a,b) where
  encodeExtent (a,b) = do encA <- encodeExtent a
                          encB <- encodeExtent b
                          Just $ \f -> encA f <> encB f
  decodeExtent = (,) <$> decodeExtent <*> decodeExtent


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Monoid which is used to fill buffers for calling HDF5 functions.
newtype DSpaceWriter = DSpaceWriter
  (  Ptr HSize -- Pointer to size
  -> Ptr HSize -- Pointer to maxsize
  -> Int         -- Guard ptr
  -> Int
  -> IO Int
  )

instance Semigroup DSpaceWriter where
  DSpaceWriter f <> DSpaceWriter g = DSpaceWriter $ \p_sz p_max i_max ->
    g p_sz p_max i_max <=< f p_sz p_max i_max

instance Monoid DSpaceWriter where
  {-# INLINE mempty #-}
  mempty = DSpaceWriter $ \_ _ _ -> pure

putDimension :: Dim -> DSpaceWriter
putDimension (Dim sz sz_max) = DSpaceWriter go where
  go p_sz p_max i_max i
    | i >= i_max = throwM $ Error [Left "Internal error: buffer overrun"]
    | otherwise  = do pokeElemOff p_sz  i (fromIntegral sz)
                      pokeElemOff p_max i (fromIntegral sz_max)
                      pure $! i + 1


----------------------------------------------------------------
-- Dataspace creation
----------------------------------------------------------------

-- | Create dataspace for a given extent
createDataspace
  :: (IsExtent dim, HasCallStack)
  => dim
  -> IO Dataspace
createDataspace dim = withFrozenCallStack $ evalContT $ do
  p_err <- ContT $ alloca
  case encodeExtent dim of
    Nothing  -> lift $ fmap Dataspace
                     $ checkHID p_err "Unable to create dataspace with NULL extent"
                     $ h5s_create H5S_NULL
    Just fld -> do
      -- First encode extents and maximum extents for given shape.
      --
      -- We hardcode maximum rank at 32 (Which was the case in HDF5 1.8)
      let DSpaceWriter write = fld putDimension
          max_rank = 32
      ptr  <- ContT $ allocaArray (max_rank * 2)
      let ptr_max = plusPtr ptr $ sz * max_rank
      rank  <- lift $ write ptr ptr_max max_rank 0
      lift $ fmap Dataspace
           $ checkHID p_err "Unable to create simple dataspace"
           $ h5s_create_simple (fromIntegral rank) ptr ptr_max
  where
    sz = sizeOf (undefined :: HSize) 

-- | Create simple dataspace which could e used in bracket-like
--   context
withCreateDataspace
  :: IsExtent dim
  => dim
  -> (Dataspace -> IO a)
  -> IO a
withCreateDataspace dim = bracket (createDataspace dim) basicClose
