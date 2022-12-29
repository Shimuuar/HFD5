{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
module HDF5.HL
  ( 
    -- * File and groups API
    File
  , open
  , withFile
  , dataset
  , withDataset
  , openAttr
  , withAttr
    -- * Datasets
  , Dataset
    -- ** Properties
  , Dim(..)
  , Extent(..)
  , dim
  , extent
    -- ** Reading and writing
  , Element(..)
  , readScalar
  , basicReadBuffer
  , basicReadScalar
  , SerializeDSet(..)
  , Serialize(..)
  , readDSet
  , read
  , write
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
  , HasAttrs
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
import HDF5.HL.Internal.Dataspace
import HDF5.C                  qualified as C
import Prelude hiding (read,readIO)

-- | Open HDF5 file. File must be closed by call to 'close'.
open :: MonadIO m => FilePath -> OpenMode -> m File
open path mode = liftIO $ withCString path $ \c_path -> do
  hid <- checkINV ("Cannot open file " ++ path)
     =<< C.h5f_open c_path (toCParam mode) C.h5p_DEFAULT
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

openAttr
  :: (MonadIO m, HasAttrs a)
  => a      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> m (Maybe Attribute)
openAttr (getHID -> hid) path = liftIO $ do
  withCString path $ \c_str -> do
    C.h5a_exists hid c_str >>= \case
      C.HFalse -> pure Nothing
      C.HTrue  -> Just . Attribute
              <$> (checkINV "Cannot open attribute" =<< C.h5a_open hid c_str C.h5p_DEFAULT)
      C.HFail  -> throwIO $ HDF5Error "Cannot check existence of attribute"

withAttr
  :: (MonadMask m, MonadIO m, HasAttrs a)
  => a      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> (Maybe Attribute -> m b)
  -> m b
withAttr a path = bracket (openAttr a path) (mapM_ close)

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


readScalar :: (Element a, HasData d, MonadIO m) => d -> m a
readScalar d = liftIO $
  bracket (getDataspaceIO d) closeIO (basicReadScalar d)

basicReadScalar
  :: forall a d. (Element a, HasData d)
  => d
  -> Dataspace
  -> IO a
basicReadScalar dset (Dataspace spc) = do
  nd <- C.h5s_get_simple_extent_ndims spc
  when (nd /= 0) $ throwIO $ HDF5Error "Scalar could only be read from scalar datasets"
  alloca $ \p -> do
    unsafeReadAll dset (typeH5 @a) (castPtr p)
    peek p

basicReadBuffer
  :: forall a d. (Element a, HasData d)
  => d
  -> Dataspace
  -> IO (VS.Vector a)
basicReadBuffer dset (Dataspace spc) = do
  n   <- C.h5s_get_simple_extent_npoints spc
  buf <- mallocForeignPtrArray (fromIntegral n)
  withForeignPtr buf $ \p -> do
    unsafeReadAll dset (typeH5 @a) (castPtr p)
    pure $! VS.unsafeFromForeignPtr0 buf (fromIntegral n)


----------------------------------------------------------------
-- Type class for datasets
----------------------------------------------------------------

-- | Data type which could be serialized to HDF5 dataset. 
class Element (ElementOf a) => SerializeDSet a where
  type ElementOf a
  -- | Read object using object itself and dataspace associated with
  --   it. This method shouldn't be called directly
  basicReadDSet :: Dataset -> Dataspace -> IO a
  default basicReadDSet :: (Serialize a) => Dataset -> Dataspace -> IO a
  basicReadDSet = basicRead
  -- | Write object to HDF5 file. At this point dataset is already
  --   created with correct dataspace.
  basicWriteDSet :: a -> Dataset -> IO ()
  default basicWriteDSet :: (Serialize a) => a -> Dataset -> IO ()
  basicWriteDSet = basicWrite
  -- | Rank of underlying array
  rank :: a -> Int
  -- | Compute dimensions of an array
  getExtent :: Monoid m => (Int -> m) -> a -> m

-- | More restrictive version which could be used for both
class SerializeDSet a => Serialize a where
  basicRead  :: HasData d => d -> Dataspace -> IO a
  basicWrite :: HasData d => a -> d -> IO ()

-- | Read data from already opened dataset. This function work
--   specifically with datasets and can use its attributes. Use 'read'
--   to be able to read from attributes as well.
readDSet :: (SerializeDSet a, MonadIO m) => Dataset -> m a
readDSet d = liftIO $ bracket (getDataspaceIO d) closeIO (basicReadDSet d)

-- | Read value from already opened dataset or attribute.
read :: (Serialize a, HasData d, MonadIO m) => d -> m a
read d = liftIO $ bracket (getDataspaceIO d) closeIO (basicRead d)

-- writeDSet :: (SerializeDSet a, MonadIO m) => a -> m ()
-- writeDSet d = liftIO $ bracket (getDataspaceIO d) closeIO (basicReadDSet d)

write
  :: forall a dir m.
     (SerializeDSet a, IsDirectory dir, MonadIO m)
  => dir -> FilePath -> a -> m ()
write (getHID -> dir) path a = liftIO $ evalContT $ do
  c_path <- ContT $ withCString path
  space  <- ContT $ withDSpace (rank a) $ getExtent putDimension a
  tid    <- ContT $ withType (typeH5 @(ElementOf a))
  -- FIXME: Variant which returns Dataset
  dset   <- ContT $ bracket
    ( checkINV "Unable to create dataset"
    =<< C.h5d_create dir c_path tid (getHID space)
        C.h5p_DEFAULT
        C.h5p_DEFAULT
        C.h5p_DEFAULT
    ) C.h5d_close
  liftIO $ basicWriteDSet a (Dataset dset)

instance Element a => SerializeDSet [a] where
  type ElementOf [a] = a
  rank      _ = 1
  getExtent f = f . length
instance Element a => Serialize     [a] where
  basicRead dset spc = VS.toList <$> basicRead dset spc
  basicWrite xs dset = basicWrite (VS.fromList xs) dset


instance Element a => SerializeDSet (VS.Vector a) where
  type ElementOf (VS.Vector a) = a
  rank      _ = 1
  getExtent f = f . VS.length
instance Element a => Serialize     (VS.Vector a) where
  basicRead dset spc = do
    n <- dataspaceDim spc
    when (n /= 1) $ error "Invalid dimention"
    basicReadBuffer dset spc
  basicWrite xs dset = do
    VS.unsafeWith xs $ \ptr ->
      unsafeWriteAll dset (typeH5 @a) (castPtr ptr)


    
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
