{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
-- |
module HDF5.HL
  ( -- * File API (H5F)
    File
  , open
  , withFile
    -- * Dataset API
  , dataset
  , withDataset
  , datasetType
  , Dim(..)
  , Extent(..)
  , getDataspace
  , getDataspaceDim

  , Element(..)
  , read
    -- * Error handling
  , HDF5Error(..)
    -- * Type classes
  , Closable(..)
  , close
  ) where

import Control.Exception      (throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Coerce
import Data.Vector.Storable     qualified as VS
import Control.Monad.Trans.Cont
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.ForeignPtr
import Data.Int
import Data.Word
import HDF5.HL.CCall
import HDF5.HL.Types
import HDF5.C                  qualified as C
import Prelude hiding (read)

-- | Open HDF5 file
open :: MonadIO m => FilePath -> OpenMode -> m File
open path mode = liftIO $ withCString path $ \c_path -> do
  hid <- C.h5f_open c_path (toCParam mode) C.h5p_DEFAULT
  when (hid == C.h5i_INVALID_HID)
    $ throwIO $ HDF5Error $ "Cannot open file " ++ path
  pure $ File hid

withFile
  :: (MonadMask m, MonadIO m)
  => FilePath -- ^ File path
  -> OpenMode
  -> (File -> m a)
  -> m a
withFile path mode = bracket (open path mode) close


-- | Open dataset
dataset
  :: (MonadIO m, Directory dir)
  => dir      -- ^ Root
  -> FilePath -- ^ Path relative to root
  -> m Dataset
dataset (directoryHID -> hid) path = liftIO $ do
  withCString path $ \c_path -> do
    r <- C.h5d_open2 hid c_path C.h5p_DEFAULT
    when (r == C.h5i_INVALID_HID)
      $ throwIO $ HDF5Error $ "Cannot open dataset " ++ path
    pure $ Dataset r

withDataset
  :: (MonadMask m, MonadIO m, Directory dir)
  => dir      -- ^ Root
  -> FilePath -- ^ Path relative to root
  -> (Dataset -> m a)
  -> m a
withDataset dir path = bracket (dataset dir path) close

-- | Return type associated with dataset
datasetType
  :: (MonadIO m)
  => Dataset
  -> m Type
datasetType (Dataset hid) = liftIO $ do
  bracket (C.h5d_get_type hid) C.h5t_close $ \ty -> do
    (fromCEnum <$> C.h5t_get_class ty) >>= \case
      Just Integer -> do
        -- FIXME: error handling
        Just sign <- fromCEnum <$> C.h5t_get_sign ty
        sz   <- C.h5t_get_precision ty
        pure $ Integral sign (fromIntegral sz)
      Just c       -> error $ "Cannot handle type " ++ show c
      Nothing      -> error "Undecodable type"

data Dim = Dim
  { dimSize    :: !Int64
  , dimMaxSize :: !Int64
  }
  deriving stock (Show,Eq,Ord)

newtype Extent = Extent [Dim]
  deriving stock (Show,Eq,Ord)

getDataspaceDim
  :: (MonadIO m)
  => Dataset
  -> m Int
getDataspaceDim (Dataset hid) = liftIO $ evalContT $ do
  spc   <- usingDataspace (C.h5d_get_space hid)
  ndim  <- liftIO $ fromIntegral <$> C.h5s_get_simple_extent_ndims spc
  if | ndim < 0  -> error "Can't get dimensions"
     | otherwise -> pure ndim


getDataspace
  :: (MonadIO m)
  => Dataset
  -> m Extent
getDataspace (Dataset hid) = liftIO $ evalContT $ do
  spc   <- usingDataspace (C.h5d_get_space hid)
  ndim  <- liftIO $ fromIntegral <$> C.h5s_get_simple_extent_ndims spc
  if | ndim < 0  -> error "getDataspace: Cannot obtain dataspace dimension"
     | ndim == 0 -> pure (Extent [])
     | otherwise -> do
         p_dim <- ContT $ allocaArray ndim
         p_max <- ContT $ allocaArray ndim
         liftIO $ do
           _ <- C.h5s_get_simple_extent_dims spc p_dim p_max
           Extent <$> sequence
             [ Dim <$> (fromIntegral <$> peekElemOff p_dim i)
                   <*> (fromIntegral <$> peekElemOff p_max i)
             | i <- [0 .. ndim-1]
             ]



----------------------------------------------------------------
-- Reading objects from dataset
----------------------------------------------------------------

-- | Data type which could be used as
class Element a where
  typeH5   :: Type
  sizeOfH5 :: Int
  peekH5   :: Ptr a -> IO a
  pokeH5   :: Ptr a -> a -> IO ()

newtype StoreHDF5 a = StoreHDF5 { unStoreHDF5 :: a }
  deriving stock (Show)

instance Element a => Storable (StoreHDF5 a) where
  sizeOf    _ = sizeOfH5 @a
  alignment _ = 1
  peek        = coerce (peekH5 @a)
  poke        = coerce (pokeH5 @a)

readBuffer
  :: forall a m. (Element a, MonadIO m)
  => Dataset
  -> m (VS.Vector (StoreHDF5 a))
readBuffer (Dataset hid) = liftIO $ evalContT $ do
  spc <- usingDataspace (C.h5d_get_space hid)
  tid <- usingType (typeH5 @a)
  liftIO $ mask_ $ do
    n   <- C.h5s_get_simple_extent_npoints spc
    buf <- mallocForeignPtrArray (fromIntegral n)
    withForeignPtr buf $ \p -> do
      convertHErr "FIXME: read" $ C.h5d_read hid tid
        C.h5s_ALL
        C.h5s_ALL
        C.h5p_DEFAULT
        (castPtr p)
      pure $ VS.unsafeFromForeignPtr0 buf (fromIntegral n)

-- | Data type which could be serailized to HDF5 dataset
class Serialize a where
  readFromDataset :: Dataset -> IO a

read :: (Serialize a, MonadIO m) => Dataset -> m a
read = liftIO . readFromDataset


instance Element a => Serialize [a] where
  readFromDataset dset = do
    n <- getDataspaceDim dset
    when (n /= 1) $ error "Invalid dimention"
    fmap unStoreHDF5 . VS.toList <$> readBuffer dset
----------------------------------------------------------------
-- Using
----------------------------------------------------------------

usingDataspace :: IO C.HID -> ContT r IO C.HID
usingDataspace io = ContT $ bracket
  (do hid <- io
      when (hid == C.h5i_INVALID_HID) $ throwIO $ HDF5Error "Cannot open dataspace"
      pure hid
  ) C.h5s_close

usingType :: Type -> ContT r IO C.HID
usingType ty = ContT $ \cnt -> bracket
  (makeType ty)
  (\case
      TyBuiltin _   -> pure ()
      TyMade    tid -> convertHErr "FIXME" $ C.h5t_close tid)
  (\case
      TyBuiltin tid -> cnt tid
      TyMade    tid -> cnt tid)

----------------------------------------------------------------
-- Boilerplate
----------------------------------------------------------------

instance Element Int8 where
  typeH5   = Integral Signed 8
  sizeOfH5 = 1
  peekH5   = peek
  pokeH5   = poke
instance Element Int16 where
  typeH5   = Integral Signed 16
  sizeOfH5 = 2
  peekH5   = peek
  pokeH5   = poke
instance Element Int32 where
  typeH5   = Integral Signed 32
  sizeOfH5 = 4
  peekH5   = peek
  pokeH5   = poke
instance Element Int64 where
  typeH5   = Integral Signed 64
  sizeOfH5 = 8
  peekH5   = peek
  pokeH5   = poke

instance Element Word8 where
  typeH5   = Integral Unsigned 8
  sizeOfH5 = 1
  peekH5   = peek
  pokeH5   = poke
instance Element Word16 where
  typeH5   = Integral Unsigned 16
  sizeOfH5 = 2
  peekH5   = peek
  pokeH5   = poke
instance Element Word32 where
  typeH5   = Integral Unsigned 32
  sizeOfH5 = 4
  peekH5   = peek
  pokeH5   = poke
instance Element Word64 where
  typeH5   = Integral Unsigned 64
  sizeOfH5 = 8
  peekH5   = peek
  pokeH5   = poke
