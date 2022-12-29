{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
module HDF5.HL
  (
    -- * File and groups API
    File
  , OpenMode(..)
  , open
  , withFile
  , openDataset
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
  , SerializeDSet(..)
  , Serialize(..)
  , readDSet
  , read
  , readAt
  , writeAt
  , SerializeAttr(..)
  , readAttr
  , writeAttr
    -- ** Attributes
  , Attribute
  , openAttr
  , createAttr
  , withAttr
    -- ** Low-level API
  , basicReadBuffer
  , basicReadScalar
    -- ** Dataspaces
  , Dataspace
  , dataspaceDim
  , dataspaceExt
    -- * Data types
  , Type
  , tyI8, tyI16, tyI32, tyI64
  , tyU8, tyU16, tyU32, tyU64
  , tyF32, tyF64
  , pattern Array
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
import Data.Bits                   (finiteBitSize)
import Data.Functor.Identity
import Data.Vector                 qualified as V
import Data.Vector.Storable        qualified as VS
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Generic         qualified as VG
import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Unboxed   qualified as FU
import Data.Vector.Fixed.Boxed     qualified as FB
import Data.Vector.Fixed.Storable  qualified as FS
import Data.Vector.Fixed.Primitive qualified as FP
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
import HDF5.C                      qualified as C
import Prelude hiding (read,readIO)

-- | Open HDF5 file. File must be closed by call to 'close'.
open :: MonadIO m => FilePath -> OpenMode -> m File
open path mode = liftIO $ withCString path $ \c_path -> do
  hid <- checkINV ("Cannot open file " ++ path)
     =<< C.h5f_open c_path (toCParam mode) C.h5p_DEFAULT
  pure $ File hid

-- | Open file and pass handle to continuation. It will be closed when
--   continuation finish execution normally or abnormally
withFile
  :: (MonadMask m, MonadIO m)
  => FilePath -> OpenMode -> (File -> m a) -> m a
withFile path mode = bracket (open path mode) close

-- | Open dataset in given location
openDataset
  :: (MonadIO m, IsDirectory dir)
  => dir      -- ^ Root
  -> FilePath -- ^ Path relative to root
  -> m Dataset
openDataset (getHID -> hid) path = liftIO $ do
  withCString path $ \c_path -> do
    r <- C.h5d_open2 hid c_path C.h5p_DEFAULT
    when (r == C.h5i_INVALID_HID)
      $ throwIO $ HDF5Error $ "Cannot open dataset " ++ path
    pure $ Dataset r

-- | Open dataset and pass handle to continuation. Dataset will be
--   closed when continuation finish execution normally or with an
--   exception.
withDataset
  :: (MonadMask m, MonadIO m, IsDirectory dir)
  => dir      -- ^ Root
  -> FilePath -- ^ Path relative to root
  -> (Dataset -> m a)
  -> m a
withDataset dir path = bracket (openDataset dir path) close

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
-- Attributes
----------------------------------------------------------------

-- | Open attribute of object. It could be either dataset or
--   group. Returns @Nothing@ if such attribute does not exists
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

-- | Open attribute of given group or dataset and pass handle to
--   continuation. It'll be closed when continuation finish
--   execution normally or with an exception.
withAttr
  :: (MonadMask m, MonadIO m, HasAttrs a)
  => a      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> (Maybe Attribute -> m b)
  -> m b
withAttr a path = bracket (openAttr a path) (mapM_ close)

-- | Create attribute
createAttr
  :: forall a dir m. (Serialize a, HasAttrs dir, MonadIO m)
  => dir    -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> a      -- ^ Value to store in attribute
  -> m ()
createAttr dir path a = liftIO $ evalContT $ do
  c_path <- ContT $ withCString path
  space  <- ContT $ withDSpace (getRank a) $ getExtent putDimension a
  tid    <- ContT $ withType (typeH5 @(ElementOf a))
  attr   <- ContT $ bracket
    (checkINV "Cannot create attribute"
    =<< C.h5a_create (getHID dir) c_path tid (getHID space)
          C.h5p_DEFAULT
          C.h5p_DEFAULT)
    C.h5a_close
  liftIO $ basicWrite (Attribute attr) a


----------------------------------------------------------------
-- Type class for elements
----------------------------------------------------------------

-- | Data type which corresponds to some HDF data type and could read
--   from buffer using 'Storable'.
class Storable a => Element a where
  typeH5 :: Type

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
  basicWriteDSet :: Dataset -> a -> IO ()
  default basicWriteDSet :: (Serialize a) => Dataset -> a -> IO ()
  basicWriteDSet = basicWrite
  -- | Rank of underlying array
  getRank :: a -> Int
  -- | Compute dimensions of an array
  getExtent :: Monoid m => (Int -> m) -> a -> m

-- | More restrictive version which could be used for both
class SerializeDSet a => Serialize a where
  basicRead  :: HasData d => d -> Dataspace -> IO a
  basicWrite :: HasData d => d -> a -> IO ()

-- | Values which could be serialized as set of attributes
class SerializeAttr a where
  basicReadAttr  :: HasAttrs d => d -> FilePath -> IO a
  basicWriteAttr :: HasAttrs d => d -> FilePath -> a -> IO ()

readAttr :: (SerializeAttr a, HasAttrs d) => d -> IO a
readAttr d = basicReadAttr d ""

writeAttr :: (SerializeAttr a, HasAttrs d) => d -> a -> IO ()
writeAttr d = basicWriteAttr d ""

-- | Read data from already opened dataset. This function work
--   specifically with datasets and can use its attributes. Use 'read'
--   to be able to read from attributes as well.
readDSet :: (SerializeDSet a, MonadIO m) => Dataset -> m a
readDSet d = liftIO $ bracket (getDataspaceIO d) closeIO (basicReadDSet d)

-- | Read value from already opened dataset or attribute.
read :: (Serialize a, HasData d, MonadIO m) => d -> m a
read d = liftIO $ bracket (getDataspaceIO d) closeIO (basicRead d)

-- | Open dataset and read it using 'readDSet'.
readAt
  :: (SerializeDSet a, IsDirectory dir, MonadIO m)
  => dir      -- ^ File (root will be used) or group
  -> FilePath -- ^ Path to dataset
  -> m a
readAt dir path = liftIO $ withDataset dir path readDSet

-- | Write dataset to HDF5 file.
writeAt
  :: forall a dir m.
     (SerializeDSet a, IsDirectory dir, MonadIO m)
  => dir      -- ^ File (root will be used) or group
  -> FilePath -- ^ Path to dataset
  -> a        -- ^ Value to write
  -> m ()
writeAt (getHID -> dir) path a = liftIO $ evalContT $ do
  c_path <- ContT $ withCString path
  space  <- ContT $ withDSpace (getRank a) $ getExtent putDimension a
  tid    <- ContT $ withType (typeH5 @(ElementOf a))
  -- FIXME: Variant which returns Dataset
  dset   <- ContT $ bracket
    ( checkINV "Unable to create dataset"
    =<< C.h5d_create dir c_path tid (getHID space)
        C.h5p_DEFAULT
        C.h5p_DEFAULT
        C.h5p_DEFAULT
    ) C.h5d_close
  liftIO $ basicWriteDSet (Dataset dset) a


----------------------------------------------------------------
-- Serialize instances
----------------------------------------------------------------

instance Element a => SerializeDSet [a] where
  type ElementOf [a] = a
  getRank   _ = 1
  getExtent f = f . length
instance Element a => Serialize     [a] where
  basicRead  dset spc = VS.toList <$> basicRead dset spc
  basicWrite dset xs  = basicWrite dset (VS.fromList xs)

instance (Element a, VU.Unbox a) => SerializeDSet (VU.Vector a) where
  type ElementOf (VU.Vector a) = a
  getRank   _ = 1
  getExtent f = f . VU.length
instance (Element a, VU.Unbox a) => Serialize (VU.Vector a) where
  basicRead  dset spc = VG.convert <$> basicRead @(VS.Vector a) dset spc
  basicWrite dset xs  = basicWrite dset (VG.convert xs :: VS.Vector a)

instance (Element a) => SerializeDSet (V.Vector a) where
  type ElementOf (V.Vector a) = a
  getRank   _ = 1
  getExtent f = f . V.length
instance (Element a) => Serialize (V.Vector a) where
  basicRead  dset spc = VG.convert <$> basicRead @(VS.Vector a) dset spc
  basicWrite dset xs  = basicWrite dset (VG.convert xs :: VS.Vector a)


instance Element a => SerializeDSet (VS.Vector a) where
  type ElementOf (VS.Vector a) = a
  getRank   _ = 1
  getExtent f = f . VS.length
instance Element a => Serialize     (VS.Vector a) where
  basicRead dset spc = do
    n <- dataspaceDim spc
    when (n /= 1) $ error "Invalid dimention"
    basicReadBuffer dset spc
  basicWrite dset xs = do
    VS.unsafeWith xs $ \ptr ->
      unsafeWriteAll dset (typeH5 @a) (castPtr ptr)



deriving via SerializeAsScalar Int8   instance SerializeDSet Int8
deriving via SerializeAsScalar Int8   instance Serialize     Int8
deriving via SerializeAsScalar Int16  instance SerializeDSet Int16
deriving via SerializeAsScalar Int16  instance Serialize     Int16
deriving via SerializeAsScalar Int32  instance SerializeDSet Int32
deriving via SerializeAsScalar Int32  instance Serialize     Int32
deriving via SerializeAsScalar Int64  instance SerializeDSet Int64
deriving via SerializeAsScalar Int64  instance Serialize     Int64
deriving via SerializeAsScalar Word8  instance SerializeDSet Word8
deriving via SerializeAsScalar Word8  instance Serialize     Word8
deriving via SerializeAsScalar Word16 instance SerializeDSet Word16
deriving via SerializeAsScalar Word16 instance Serialize     Word16
deriving via SerializeAsScalar Word32 instance SerializeDSet Word32
deriving via SerializeAsScalar Word32 instance Serialize     Word32
deriving via SerializeAsScalar Word64 instance SerializeDSet Word64
deriving via SerializeAsScalar Word64 instance Serialize     Word64

deriving via SerializeAsScalar Int  instance SerializeDSet Int
deriving via SerializeAsScalar Int  instance Serialize     Int
deriving via SerializeAsScalar Word instance SerializeDSet Word
deriving via SerializeAsScalar Word instance Serialize     Word

deriving via SerializeAsScalar Float  instance SerializeDSet Float
deriving via SerializeAsScalar Float  instance Serialize     Float
deriving via SerializeAsScalar Double instance SerializeDSet Double
deriving via SerializeAsScalar Double instance Serialize     Double

deriving via SerializeAsScalar (FB.Vec n a)
    instance (F.Arity n, Element a) => Serialize (FB.Vec n a)
deriving via SerializeAsScalar (FB.Vec n a)
    instance (F.Arity n, Element a) => SerializeDSet (FB.Vec n a)

deriving via SerializeAsScalar (FU.Vec n a)
    instance (F.Arity n, Element a, FU.Unbox n a) => Serialize (FU.Vec n a)
deriving via SerializeAsScalar (FU.Vec n a)
    instance (F.Arity n, Element a, FU.Unbox n a) => SerializeDSet (FU.Vec n a)

deriving via SerializeAsScalar (FS.Vec n a)
    instance (F.Arity n, Element a) => Serialize (FS.Vec n a)
deriving via SerializeAsScalar (FS.Vec n a)
    instance (F.Arity n, Element a) => SerializeDSet (FS.Vec n a)

deriving via SerializeAsScalar (FP.Vec n a)
    instance (F.Arity n, Element a, FP.Prim a) => Serialize (FP.Vec n a)
deriving via SerializeAsScalar (FP.Vec n a)
    instance (F.Arity n, Element a, FP.Prim a) => SerializeDSet (FP.Vec n a)

deriving newtype instance Serialize     a => Serialize     (Identity a)
deriving newtype instance SerializeDSet a => SerializeDSet (Identity a)

-- | Newtype wrapper for derivation of serialization instances as
--   scalars.
newtype SerializeAsScalar a = SerializeAsScalar a
  deriving newtype (Storable, Element)

instance Element a => SerializeDSet (SerializeAsScalar a) where
  type ElementOf (SerializeAsScalar a) = a
  getRank   _   = 0
  getExtent _ _ = mempty

instance Element a => Serialize (SerializeAsScalar a) where
  basicRead  dset spc = basicReadScalar dset spc
  basicWrite dset a   = do
    alloca $ \p -> do poke p a
                      unsafeWriteAll dset (typeH5 @a) (castPtr p)


----------------------------------------------------------------
-- Boilerplate
----------------------------------------------------------------

instance Element Int8   where typeH5 = tyI8
instance Element Int16  where typeH5 = tyI16
instance Element Int32  where typeH5 = tyI32
instance Element Int64  where typeH5 = tyI64
instance Element Word8  where typeH5 = tyU8
instance Element Word16 where typeH5 = tyU16
instance Element Word32 where typeH5 = tyU32
instance Element Word64 where typeH5 = tyU64

instance Element Float  where typeH5 = tyF32
instance Element Double where typeH5 = tyF64

instance (Element a, F.Arity n) => Element (FB.Vec n a) where
  typeH5 = Array (typeH5 @a) [F.length (undefined :: FB.Vec n a)]
instance (Element a, F.Arity n, FU.Unbox n a) => Element (FU.Vec n a) where
  typeH5 = Array (typeH5 @a) [F.length (undefined :: FU.Vec n a)]
instance (Element a, F.Arity n) => Element (FS.Vec n a) where
  typeH5 = Array (typeH5 @a) [F.length (undefined :: FS.Vec n a)]
instance (Element a, F.Arity n, FP.Prim a) => Element (FP.Vec n a) where
  typeH5 = Array (typeH5 @a) [F.length (undefined :: FP.Vec n a)]

instance Element a => Element (Identity a) where typeH5 = typeH5 @a

instance Element Int where
  typeH5 | wordSizeInBits == 64 = tyI64
         | otherwise            = tyI32
instance Element Word where
  typeH5 | wordSizeInBits == 64 = tyU64
         | otherwise            = tyU32

wordSizeInBits :: Int
wordSizeInBits = finiteBitSize (0 :: Word)
