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
  ( 
    -- * File and groups API
    File
  , open
  , withFile
  , dataset
  , withDataset
    -- * Datasets
  , Dataset
    -- ** Properties
  , Dim(..)
  , Extent(..)
  , dim
  , extent
    -- ** Reading and writing
  , Element(..)
  , readBuffer
  , read
    -- ** Dataspaces
  , Dataspace
  , dataspaceDim
  , dataspaceExt
    -- ** Attributes
  , Attribute
    -- * Error handling
  , HDF5Error(..)
    -- * Type classes
    -- ** HDF objects
  , IsObject
  , IsDirectory
  , HasData(..)
  , getType
  , getDataspace
    -- ** Closing objects
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
import HDF5.HL.Internal.Types
import HDF5.C                  qualified as C
import Prelude hiding (read,readIO)

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


----------------------------------------------------------------
-- Dataspace API
----------------------------------------------------------------

dim :: (HasData a, MonadIO m) => a -> m Int
dim = dataspaceDim <=< getDataspace

extent :: (HasData a, MonadIO m) => a -> m Extent
extent = dataspaceExt <=< getDataspace

dataspaceDim
  :: (MonadIO m)
  => Dataspace
  -> m Int
dataspaceDim (Dataspace hid) = liftIO $ do
  ndim <- fromIntegral <$> C.h5s_get_simple_extent_ndims hid
  if | ndim < 0  -> error "Can't get dimensions"
     | otherwise -> pure ndim

dataspaceExt
  :: (MonadIO m)
  => Dataspace
  -> m Extent
dataspaceExt (Dataspace hid) = liftIO $ do
  ndim <- fromIntegral <$> C.h5s_get_simple_extent_ndims hid
  if | ndim < 0  -> error "getDataspace: Cannot obtain dataspace dimension"
     | ndim == 0 -> pure (Extent [])
     | otherwise -> evalContT $ do
         p_dim <- ContT $ allocaArray ndim
         p_max <- ContT $ allocaArray ndim
         liftIO $ do
           _ <- C.h5s_get_simple_extent_dims hid p_dim p_max
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
readBuffer dset@(Dataset hid) = liftIO $ evalContT $ do
  Dataspace spc <- ContT $ bracket (getDataspaceIO dset) close
  tid           <- ContT $ withType $ typeH5 @a
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
  readIO :: IsObject d => d -> IO a

read :: (Serialize a, MonadIO m) => Dataset -> m a
read = liftIO . readIO

instance Element a => Serialize [a] where
  readIO (castObj' -> dset) = do
    n <- dim dset
    when (n /= 1) $ error "Invalid dimention"
    VS.toList <$> readBuffer dset

instance Element a => Serialize (VS.Vector a) where
  readIO (castObj' -> dset) = do
    n <- dim dset
    when (n /= 1) $ error "Invalid dimention"
    readBuffer dset


    
----------------------------------------------------------------
-- Using
----------------------------------------------------------------



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
