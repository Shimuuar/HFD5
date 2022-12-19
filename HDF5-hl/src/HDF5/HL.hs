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
-- import Data.Coerce
import Data.Vector.Storable     qualified as VS
import Control.Monad.Trans.Cont
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.ForeignPtr
import Data.Int
import Data.Word
import HDF5.HL.Internal.CCall
import HDF5.HL.Internal.TyHDF
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
  :: (MonadIO m, IsDirectory dir)
  => dir      -- ^ Root
  -> FilePath -- ^ Path relative to root
  -> m Dataset
dataset (getHID -> hid) path = liftIO $ do
  withCString path $ \c_path -> do
    r <- C.h5d_open2 hid c_path C.h5p_DEFAULT
    when (r == C.h5i_INVALID_HID)
      $ throwIO $ HDF5Error $ "Cannot open dataset " ++ path
    pure $ Dataset r

withDataset
  :: (MonadMask m, MonadIO m, IsDirectory dir)
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
datasetType (Dataset hid) = liftIO $ unsafeNewType $ C.h5d_get_type hid

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
-- Type class for elements
----------------------------------------------------------------

-- | Data type which corresponds to some HDF data type and could read
--   from buffer using 'Storable'. 
class Storable a => Element a where
  typeH5 :: Type



----------------------------------------------------------------
-- Primitives for reading from dataset
----------------------------------------------------------------

readBuffer
  :: forall a m. (Element a, MonadIO m)
  => Dataset
  -> m (VS.Vector a)
readBuffer (Dataset hid) = liftIO $ evalContT $ do
  spc <- usingDataspace (C.h5d_get_space hid)
  tid <- ContT $ withType $ typeH5 @a
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


----------------------------------------------------------------
-- Type class for datasets
----------------------------------------------------------------

-- | Data type which could be serailized to HDF5 dataset
class Serialize a where
  readFromDataset :: Dataset -> IO a

read :: (Serialize a, MonadIO m) => Dataset -> m a
read = liftIO . readFromDataset


instance Element a => Serialize [a] where
  readFromDataset dset = do
    n <- getDataspaceDim dset
    when (n /= 1) $ error "Invalid dimention"
    VS.toList <$> readBuffer dset
    
----------------------------------------------------------------
-- Using
----------------------------------------------------------------

usingDataspace :: IO C.HID -> ContT r IO C.HID
usingDataspace io = ContT $ bracket
  (do hid <- io
      when (hid == C.h5i_INVALID_HID) $ throwIO $ HDF5Error "Cannot open dataspace"
      pure hid
  ) C.h5s_close


----------------------------------------------------------------
-- Boilerplate
----------------------------------------------------------------

instance Element Int8   where typeH5 = Native C.h5t_NATIVE_SCHAR
instance Element Int16  where typeH5 = Native C.h5t_NATIVE_SHORT
instance Element Int32  where typeH5 = Native C.h5t_NATIVE_INT
instance Element Int64  where typeH5 = Native C.h5t_NATIVE_LONG
instance Element Word8  where typeH5 = Native C.h5t_NATIVE_UCHAR
instance Element Word16 where typeH5 = Native C.h5t_NATIVE_USHORT
instance Element Word32 where typeH5 = Native C.h5t_NATIVE_UINT
instance Element Word64 where typeH5 = Native C.h5t_NATIVE_ULONG

instance Element Float  where typeH5 = Native C.h5t_NATIVE_FLOAT
instance Element Double where typeH5 = Native C.h5t_NATIVE_DOUBLE
