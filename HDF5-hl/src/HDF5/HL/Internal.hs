{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- All modules inside @HDF5.HL.Internal@ constitute internal API.
-- It considered part of public API but its stability is of little
-- concert and shouldn't be relied upon.
--
-- All operations of internal API are done in 'HIO' monad which is
-- just a wrapper over @IO@ and used to ensure that call to HDF5 are
-- protected by mutex.
module HDF5.HL.Internal where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Complex                (Complex)
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
import GHC.Stack

import HDF5.HL.Internal.Types
import HDF5.HL.Internal.Wrappers
import HDF5.HL.Internal.Error
import HDF5.HL.Internal.Dataspace
import HDF5.HL.Vector
import HDF5.C
import Prelude hiding (read,readIO)



-- | Read value from already opened dataset or attribute.
basicReadObject :: (Serialize a, HasData d, MonadIO m, HasCallStack) => d -> m a
basicReadObject d = liftIO $ withDataspace d $ \spc -> basicRead d spc


dataspaceRank
  :: (HasCallStack)
  => Dataspace
  -> IO (Maybe Int)
dataspaceRank (Dataspace hid)
  = withFrozenCallStack
  $ alloca $ \p_err ->
    h5s_get_simple_extent_type hid p_err >>= \case
      H5S_NULL   -> pure   Nothing
      H5S_SCALAR -> pure $ Just 0
      H5S_SIMPLE -> do
        n <- checkCInt p_err "Cannot get rank of dataspace's extent"
           $ h5s_get_simple_extent_ndims hid
        pure $ Just (fromIntegral n)
      _ -> throwM =<< decodeError p_err "Cannot get dataspace type"


----------------------------------------------------------------
-- Attributes
----------------------------------------------------------------

openAttr
  :: (HasAttrs a, HasCallStack)
  => a      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> IO (Maybe Attribute)
openAttr (getHID -> hid) path = withFrozenCallStack $ evalContT $ do
  p_err <- ContT $ alloca
  c_str <- ContT $ withCString path
  lift $ do
    exists <- checkHTri p_err ("Cannot check whether attribute " ++ path ++ " exists")
            $ h5a_exists hid c_str
    case exists of
      False -> pure Nothing
      True  -> Just . Attribute
            <$> ( checkHID p_err ("Cannot open attribute " ++ path)
                $ h5a_open hid c_str H5P_DEFAULT)

withAttr
  :: (HasAttrs a, HasCallStack)
  => a      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> (Maybe Attribute -> IO b)
  -> IO b
withAttr a path = bracket (openAttr a path) (mapM_ basicClose)

basicCreateAttr
  :: forall a dir. (Serialize a, HasAttrs dir, HasCallStack)
  => dir    -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> a      -- ^ Value to store in attribute
  -> IO ()
basicCreateAttr dir path a = evalContT $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  space  <- ContT $ withCreateDataspace (getExtent a) Nothing
  tid    <- ContT $ withType $ typeH5 @(ElementOf a)
  attr   <- ContT $ bracket
    ( withFrozenCallStack
    $ fmap Attribute
    $ checkHID p_err ("Cannot create attribute " ++ path)
    $ h5a_create (getHID dir) c_path tid (getHID space)
          H5P_DEFAULT
          H5P_DEFAULT)
    basicClose
  lift $ basicWrite attr a

basicReadAttr
  :: (Serialize a, HasAttrs d, HasCallStack)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> IO (Maybe a)
basicReadAttr a name = withAttr a name $ \case
  Just x  -> Just <$> basicReadObject x
  Nothing -> pure Nothing


----------------------------------------------------------------
-- Type class for datasets
----------------------------------------------------------------

-- | Data type which could be serialized to HDF5 dataset.
class (Element (ElementOf a), IsExtent (ExtentOf a)) => Serialize a where
  type ElementOf a
  type ExtentOf  a
  -- | Read object using object itself and dataspace associated with
  --   it. This method shouldn't be called directly
  basicRead :: (HasCallStack, HasData d) => d -> Dataspace -> IO a
  -- | Write object to HDF5 file. At this point dataset is already
  --   created with correct dataspace.
  basicWrite :: (HasCallStack, HasData d) => d -> a -> IO ()
  -- | Compute dimensions of an array
  getExtent :: a -> ExtentOf a

-- | Data types which could be read and written using simple hyperslabs
class Serialize a => SerializeSlab a where
  -- | Read part of dataset into given data structure
  basicReadSlab :: Dataset
                -> ExtentOf a -- ^ Offset into on-disk data structure
                -> ExtentOf a -- ^ Size of data to read
                -> IO a
  -- | Write content of array to a dataspace
  basicWriteSlab :: Dataset    -- ^ Dataset to write to
                 -> ExtentOf a -- ^ Offset into dataset
                 -> a          -- ^ Data to write
                 -> IO ()


-- | Values which could be serialized as set of attributes
class SerializeAttr a where
  basicFromAttrs :: HasCallStack => AttributeM a
  basicToAttrs   :: HasCallStack => a -> AttributeM ()

instance SerializeAttr () where
  basicToAttrs   = pure
  basicFromAttrs = pure ()


newtype AttributeM a = AttributeM
  { unAttributeM :: forall d. HasAttrs d => d -> (FilePath -> FilePath) -> IO a }
  deriving Functor

runAttributeM :: HasAttrs d => d -> AttributeM a -> IO a
runAttributeM d (AttributeM f) = f d id

instance Applicative AttributeM where
  pure a = AttributeM $ \_ _ -> pure a
  (<*>)  = ap

instance Monad AttributeM where
  m >>= fun = AttributeM $ \d f -> do
    a <- unAttributeM m d f
    unAttributeM (fun a) d f

basicAttrSubset :: FilePath -> AttributeM a -> AttributeM a
basicAttrSubset dir m = AttributeM $ \d fun -> unAttributeM m d ((dir++) . ('/':) . fun)

basicEncodeAttr :: Serialize a => FilePath -> a -> AttributeM ()
basicEncodeAttr name a = AttributeM $ \d fun -> do
  basicCreateAttr d (fun name) a

basicDecodeAttr :: Serialize a => FilePath -> AttributeM a
basicDecodeAttr name = AttributeM $ \d fun -> do
  basicReadAttr d (fun name) >>= \case
    Nothing -> error "No attribute" -- FIXME: proper error handling
    Just a  -> pure a



instance (Element a) => Serialize [a] where
  type ElementOf [a] = a
  type ExtentOf  [a] = Int
  getExtent = length
  basicRead  dset spc = VG.toList <$> basicRead @(VecHDF5 a) dset spc
  basicWrite dset xs  = basicWrite dset (VG.fromList @VecHDF5 xs)

instance (Element a) => SerializeSlab [a] where
  basicReadSlab  dset off sz = VG.toList <$> basicReadSlab @(VecHDF5 a) dset off sz
  basicWriteSlab dset off xs = basicWriteSlab dset off (VG.fromList @VecHDF5 xs)


instance (Element a, VU.Unbox a) => Serialize (VU.Vector a) where
  type ElementOf (VU.Vector a) = a
  type ExtentOf  (VU.Vector a) = Int
  getExtent = VU.length
  basicRead  dset spc = VG.convert <$> basicRead @(VecHDF5 a) dset spc
  basicWrite dset xs  = basicWrite dset (VG.convert xs :: VecHDF5 a)

instance (Element a, VU.Unbox a) => SerializeSlab (VU.Vector a) where
  basicReadSlab  dset off sz = VG.convert <$> basicReadSlab @(VecHDF5 a) dset off sz
  basicWriteSlab dset off xs = basicWriteSlab dset off (VG.convert xs :: VecHDF5 a)


instance (Element a) => Serialize (V.Vector a) where
  type ElementOf (V.Vector a) = a
  type ExtentOf  (V.Vector a) = Int
  getExtent = V.length
  basicRead  dset spc = VG.convert <$> basicRead @(VecHDF5 a) dset spc
  basicWrite dset xs  = basicWrite dset (VG.convert xs :: VecHDF5 a)

instance (Element a) => SerializeSlab (V.Vector a) where
  basicReadSlab  dset off sz = VG.convert <$> basicReadSlab @(VecHDF5 a) dset off sz
  basicWriteSlab dset off xs = basicWriteSlab dset off (VG.convert xs :: VecHDF5 a)


instance (Storable a, Element a) => Serialize (VS.Vector a) where
  type ElementOf (VS.Vector a) = a
  type ExtentOf  (VS.Vector a) = Int
  getExtent = VS.length
  basicRead  dset spc = VG.convert <$> basicRead @(VS.Vector a) dset spc
  basicWrite dset xs  = basicWrite dset (VG.convert xs :: VS.Vector a)

instance (Storable a, Element a) => SerializeSlab (VS.Vector a) where
  basicReadSlab  dset off sz = VG.convert <$> basicReadSlab @(VecHDF5 a) dset off sz
  basicWriteSlab dset off xs = basicWriteSlab dset off (VG.convert xs :: VecHDF5 a)


----------------------------------------------------------------
-- Vector with HDF5 encoding
----------------------------------------------------------------

instance (Element a) => Serialize (VecHDF5 a) where
  type ElementOf (VecHDF5 a) = a
  type ExtentOf  (VecHDF5 a) = Int
  getExtent = VG.length
  basicRead dset spc = do
    n <- dataspaceRank spc
    when (n /= Just 1) $ error "Invalid dimention"
    basicReadBuffer dset spc
  basicWrite dset xs = unsafeWithH5 xs $ unsafeWriteAll dset (typeH5 @a)


instance (Element a) => SerializeSlab (VecHDF5 a) where
  basicReadSlab dset off sz = evalContT $ do
    p_err <- ContT alloca
    -- File dataspace
    spc_file <- lift $ getDataspaceIO dset
    lift $ setSlabSelection spc_file off sz
    -- Memory dataspace
    spc_mem <- ContT $ withCreateDataspace sz Nothing
    -- Prepare reading
    tid     <- ContT $ withType (typeH5 @a)
    buf     <- lift  $ mallocVectorH5 sz
    ptr     <- ContT $ withForeignPtr buf
    lift $ checkHErr p_err "Reading dataset data failed"
         $ h5d_read (getHID dset) tid
             (getHID spc_mem) (getHID spc_file)
             H5P_DEFAULT (castPtr ptr)
    pure $! unsafeFromForeignPtr buf sz
  --
  basicWriteSlab dset off vec = evalContT $ do
    p_err <- ContT alloca
    -- File dataspace
    spc_file <- lift $ getDataspaceIO dset
    lift $ setSlabSelection spc_file off sz
    -- Memory dataspace
    spc_mem <- ContT $ withCreateDataspace sz Nothing
    -- Writing
    tid <- ContT $ withType (typeH5 @a)
    ptr <- ContT $ unsafeWithH5 vec
    lift $ checkHErr p_err "Writing dataset data failed"
         $ h5d_write (getHID dset) tid
             (getHID spc_mem) (getHID spc_file) H5P_DEFAULT ptr
    where
      sz = VG.length vec


deriving via SerializeAsScalar Int8   instance Serialize Int8
deriving via SerializeAsScalar Int16  instance Serialize Int16
deriving via SerializeAsScalar Int32  instance Serialize Int32
deriving via SerializeAsScalar Int64  instance Serialize Int64
deriving via SerializeAsScalar Word8  instance Serialize Word8
deriving via SerializeAsScalar Word16 instance Serialize Word16
deriving via SerializeAsScalar Word32 instance Serialize Word32
deriving via SerializeAsScalar Word64 instance Serialize Word64

deriving via SerializeAsScalar Int  instance Serialize Int
deriving via SerializeAsScalar Word instance Serialize Word

deriving via SerializeAsScalar Float  instance Serialize Float
deriving via SerializeAsScalar Double instance Serialize Double

deriving via SerializeAsScalar (Complex a)
    instance Element a => Serialize (Complex a)

deriving via SerializeAsScalar (FB.Vec n a)
    instance (F.Arity n, Element a) => Serialize (FB.Vec n a)
deriving via SerializeAsScalar (FU.Vec n a)
    instance (F.Arity n, Element a, FU.Unbox n a) => Serialize (FU.Vec n a)
deriving via SerializeAsScalar (FS.Vec n a)
    instance (F.Arity n, Element a, Storable a) => Serialize (FS.Vec n a)
deriving via SerializeAsScalar (FP.Vec n a)
    instance (F.Arity n, Element a, FP.Prim a) => Serialize (FP.Vec n a)

deriving newtype instance Serialize    a => Serialize    (Identity a)

-- | Newtype wrapper for derivation of serialization instances as
--   scalars.
newtype SerializeAsScalar a = SerializeAsScalar a
  deriving newtype (Storable, Element)

instance Element a => Serialize (SerializeAsScalar a) where
  type ElementOf (SerializeAsScalar a) = a
  type ExtentOf  (SerializeAsScalar a) = ()
  getExtent _ = ()
  basicRead  dset spc = basicReadScalar dset spc
  basicWrite dset a   = evalContT $ do
    p  <- ContT $ allocaElement
    lift $ do pokeH5 p a
              unsafeWriteAll dset (typeH5 @a) p

----------------------------------------------------------------
-- Primitive read/write operations
----------------------------------------------------------------

basicReadBuffer
  :: forall a d. (Element a, HasData d, HasCallStack)
  => d
  -> Dataspace
  -> IO (VecHDF5 a)
basicReadBuffer dset (Dataspace spc) = do
  alloca $ \p_err -> do
    n   <- withFrozenCallStack
         $ checkCLLong p_err "Cannot get number of points for dataspace"
         $ h5s_get_simple_extent_npoints spc
    buf <- mallocVectorH5 (fromIntegral n)
    evalContT $ do
      p  <- ContT $ withForeignPtr buf
      lift $ do unsafeReadAll dset (typeH5 @a) (castPtr p)
                pure $! unsafeFromForeignPtr buf (fromIntegral n)

basicReadScalar
  :: forall a d. (Element a, HasData d, HasCallStack)
  => d
  -> Dataspace
  -> IO a
basicReadScalar dset spc = do
  dataspaceRank spc >>= \case
    Nothing -> throwM $ Error [Left "Cannot read scalar from NULL dataset"]
    Just 0  -> pure ()
    Just _  -> throwM $ Error [Left "Cannot read scalar from non-scalar dataset"]
  allocaElement $ \p -> do unsafeReadAll dset (typeH5 @a) p
                           peekH5 p
