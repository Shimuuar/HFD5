{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
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
import HDF5.HL.Internal.TyHDF
import HDF5.HL.Internal.Types
import HDF5.HL.Internal.Error
import HDF5.HL.Internal.Dataspace
import HDF5.HL.Internal.Enum
import HDF5.C
import Prelude hiding (read,readIO)

----------------------------------------------------------------
-- File API
----------------------------------------------------------------

openFile :: FilePath -> OpenMode -> HIO File
openFile path mode =
  hioWithCString path $ \c_path -> do
    hid <- checkHID ("Cannot open file " ++ path)
       =<< h5f_open c_path (toCParam mode) h5p_DEFAULT
    pure $ File hid

createFile :: FilePath -> CreateMode -> HIO File
createFile path mode =
  hioWithCString path $ \c_path -> do
    hid <- checkHID ("Cannot create file " ++ path)
       =<< h5f_create c_path (toCParam mode) h5p_DEFAULT h5p_DEFAULT
    pure $ File hid

----------------------------------------------------------------
-- Dataset API
----------------------------------------------------------------

openDataset
  :: (IsDirectory dir)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> HIO Dataset
openDataset (getHID -> hid) path = do
  hioWithCString path $ \c_path -> do
    r <- checkHID ("Cannot open dataset " ++ path)
     =<< h5d_open2 hid c_path h5p_DEFAULT
    pure $ Dataset r

createEmptyDataset
  :: ( IsDirectory dir, IsExtent ext)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> Type     -- ^ Element type
  -> ext      -- ^ Dataspace, that is size of dataset
  -> HIO Dataset
createEmptyDataset (getHID -> hid) path ty ext = evalContT $ do
  c_path <- ContT $ hioWithCString path
  space  <- ContT $ withDSpace     ext
  lift $ fmap Dataset
       $ checkHID "Unable to create dataset"
     =<< h5d_create hid c_path (getHID ty) (getHID space)
         h5p_DEFAULT
         h5p_DEFAULT
         h5p_DEFAULT

basicCreateDataset
  :: forall a dir.
     (SerializeDSet a, IsDirectory dir)
  => dir      -- ^ File (root will be used) or group
  -> FilePath -- ^ Path to dataset
  -> a        -- ^ Value to write
  -> HIO ()
basicCreateDataset dir path a = evalContT $ do
  ty   <- ContT $ withType @(ElementOf a)
  dset <- ContT $ withCreateEmptyDataset dir path ty (getExtent a)
  lift $ basicWriteDSet dset a


withOpenDataset
  :: (IsDirectory dir)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> (Dataset -> HIO a)
  -> HIO a
withOpenDataset dir path = bracket (openDataset dir path) basicClose

withCreateEmptyDataset
  :: ( IsDirectory dir, IsExtent ext)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> Type     -- ^ Element type
  -> ext      -- ^ Dataspace, that is size of dataset
  -> (Dataset -> HIO a)
  -> HIO a
withCreateEmptyDataset dir path ty ext = bracket
  (createEmptyDataset dir path ty ext)
  basicClose

readDataset :: (SerializeDSet a) => Dataset -> HIO a
readDataset d = withDataspace d $ \spc -> basicReadDSet d spc

readObject :: (Serialize a, HasData d) => d -> HIO a
readObject d = withDataspace d $ \spc -> basicRead d spc

----------------------------------------------------------------
-- File API
----------------------------------------------------------------

openGroup
  :: (IsDirectory dir)
  => dir
  -> FilePath
  -> HIO Group
openGroup (getHID -> hid) path = 
  hioWithCString path $ \c_path -> do
    r <- checkHID "Cannot open group" =<< h5g_open hid c_path h5p_DEFAULT
    pure $ Group r

withOpenGroup
  :: (IsDirectory dir)
  => dir
  -> FilePath
  -> (Group -> HIO a)
  -> HIO a
withOpenGroup dir path = bracket (openGroup dir path) basicClose

createGroup
  :: (IsDirectory dir)
  => dir
  -> FilePath
  -> HIO Group
createGroup (getHID -> hid) path =
  hioWithCString path $ \c_path -> do
    r <- checkHID "Cannot open group"
     =<< h5g_create hid c_path h5p_DEFAULT h5p_DEFAULT h5p_DEFAULT
    pure $ Group r

withCreateGroup
  :: (IsDirectory dir)
  => dir
  -> FilePath
  -> (Group -> HIO a)
  -> HIO a
withCreateGroup dir path = bracket (createGroup dir path) basicClose



----------------------------------------------------------------
-- Dataspace
----------------------------------------------------------------

dataspaceRank
  :: Dataspace
  -> HIO (Maybe Int)
dataspaceRank (Dataspace hid) = do
  h5s_get_simple_extent_type hid >>= \case
    H5S_NULL   -> pure   Nothing
    H5S_SCALAR -> pure $ Just 0
    H5S_SIMPLE -> do
      n <- h5s_get_simple_extent_ndims hid
      when (n < 0) $ error "Cannot parse extent"
      pure $ Just (fromIntegral n)
    _ -> error "getDataspace: Cannot obtain dataspace dimension"

----------------------------------------------------------------
-- Attributes
----------------------------------------------------------------

openAttr
  :: (HasAttrs a)
  => a      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> HIO (Maybe Attribute)
openAttr (getHID -> hid) path = do
  hioWithCString path $ \c_str -> do
    h5a_exists hid c_str >>= \case
      HFalse -> pure Nothing
      HTrue  -> Just . Attribute
            <$> (checkHID "Cannot open attribute" =<< h5a_open hid c_str h5p_DEFAULT)
      HFail  -> throwM =<< decodeError "Cannot check existence of attribute"

withAttr
  :: (HasAttrs a)
  => a      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> (Maybe Attribute -> HIO b)
  -> HIO b
withAttr a path = bracket (openAttr a path) (mapM_ basicClose)

basicCreateAttr
  :: forall a dir. (Serialize a, HasAttrs dir)
  => dir    -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> a      -- ^ Value to store in attribute
  -> HIO ()
basicCreateAttr dir path a = evalContT $ do
  c_path <- ContT $ hioWithCString path
  space  <- ContT $ withDSpace (getExtent a)
  ty     <- ContT $ withType @(ElementOf a)
  attr   <- ContT $ bracket
    (checkHID "Cannot create attribute"
    =<< h5a_create (getHID dir) c_path (getHID ty) (getHID space)
          h5p_DEFAULT
          h5p_DEFAULT)
    h5a_close
  lift $ basicWrite (Attribute attr) a

basicReadAttr
  :: (Serialize a, HasAttrs d)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> HIO (Maybe a)
basicReadAttr a name = withAttr a name $ \case
  Just x  -> Just <$> readObject x
  Nothing -> pure Nothing


----------------------------------------------------------------
-- Type class for elements
----------------------------------------------------------------

-- | Data type which corresponds to some HDF data type and could read
--   from buffer using 'Storable'.
class Storable a => Element a where
  typeH5 :: HIO Type

withType :: forall a r. Element a => (Type -> HIO r) -> HIO r
withType = bracket (typeH5 @a) basicClose

instance Element Int8   where typeH5 = pure tyI8
instance Element Int16  where typeH5 = pure tyI16
instance Element Int32  where typeH5 = pure tyI32
instance Element Int64  where typeH5 = pure tyI64
instance Element Word8  where typeH5 = pure tyU8
instance Element Word16 where typeH5 = pure tyU16
instance Element Word32 where typeH5 = pure tyU32
instance Element Word64 where typeH5 = pure tyU64

instance Element Float  where typeH5 = pure tyF32
instance Element Double where typeH5 = pure tyF64

instance (Element a, F.Arity n) => Element (FB.Vec n a) where
  typeH5 = do ty <- typeH5 @a
              makeArray ty [F.length (undefined :: FB.Vec n a)]
instance (Element a, F.Arity n, FU.Unbox n a) => Element (FU.Vec n a) where
  typeH5 = do ty <- typeH5 @a
              makeArray ty [F.length (undefined :: FB.Vec n a)]
instance (Element a, F.Arity n) => Element (FS.Vec n a) where
  typeH5 = do ty <- typeH5 @a
              makeArray ty [F.length (undefined :: FB.Vec n a)]
instance (Element a, F.Arity n, FP.Prim a) => Element (FP.Vec n a) where
  typeH5 = do ty <- typeH5 @a
              makeArray ty [F.length (undefined :: FB.Vec n a)]

instance Element a => Element (Identity a) where typeH5 = typeH5 @a

instance Element Int where
  typeH5 | wordSizeInBits == 64 = pure tyI64
         | otherwise            = pure tyI32
instance Element Word where
  typeH5 | wordSizeInBits == 64 = pure tyU64
         | otherwise            = pure tyU32

wordSizeInBits :: Int
wordSizeInBits = finiteBitSize (0 :: Word)


----------------------------------------------------------------
-- Type class for datasets
----------------------------------------------------------------

-- | Data type which could be serialized to HDF5 dataset.
class (Element (ElementOf a), IsExtent (ExtentOf a)) => SerializeDSet a where
  type ElementOf a
  type ExtentOf  a
  -- | Read object using object itself and dataspace associated with
  --   it. This method shouldn't be called directly
  basicReadDSet :: Dataset -> Dataspace -> HIO a
  default basicReadDSet :: (Serialize a) => Dataset -> Dataspace -> HIO a
  basicReadDSet = basicRead
  -- | Write object to HDF5 file. At this point dataset is already
  --   created with correct dataspace.
  basicWriteDSet :: Dataset -> a -> HIO ()
  default basicWriteDSet :: (Serialize a) => Dataset -> a -> HIO ()
  basicWriteDSet = basicWrite
  -- | Compute dimensions of an array
  getExtent :: a -> ExtentOf a

-- | More restrictive version which could be used for both
class SerializeDSet a => Serialize a where
  basicRead  :: HasData d => d -> Dataspace -> HIO a
  basicWrite :: HasData d => d -> a -> HIO ()

-- | Values which could be serialized as set of attributes
class SerializeAttr a where
  basicFromAttrs :: AttributeM a
  basicToAttrs :: a -> AttributeM ()

newtype AttributeM a = AttributeM
  { unAttributeM :: forall d. HasAttrs d => d -> (FilePath -> FilePath) -> HIO a }
  deriving Functor

runAttributeM :: HasAttrs d => d -> AttributeM a -> HIO a
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



instance Element a => SerializeDSet [a] where
  type ElementOf [a] = a
  type ExtentOf  [a] = Int
  getExtent = length
instance Element a => Serialize     [a] where
  basicRead  dset spc = VS.toList <$> basicRead dset spc
  basicWrite dset xs  = basicWrite dset (VS.fromList xs)

instance (Element a, VU.Unbox a) => SerializeDSet (VU.Vector a) where
  type ElementOf (VU.Vector a) = a
  type ExtentOf  (VU.Vector a) = Int
  getExtent = VU.length
instance (Element a, VU.Unbox a) => Serialize (VU.Vector a) where
  basicRead  dset spc = VG.convert <$> basicRead @(VS.Vector a) dset spc
  basicWrite dset xs  = basicWrite dset (VG.convert xs :: VS.Vector a)

instance (Element a) => SerializeDSet (V.Vector a) where
  type ElementOf (V.Vector a) = a
  type ExtentOf  (V.Vector a) = Int
  getExtent = V.length
instance (Element a) => Serialize (V.Vector a) where
  basicRead  dset spc = VG.convert <$> basicRead @(VS.Vector a) dset spc
  basicWrite dset xs  = basicWrite dset (VG.convert xs :: VS.Vector a)


instance Element a => SerializeDSet (VS.Vector a) where
  type ElementOf (VS.Vector a) = a
  type ExtentOf  (VS.Vector a) = Int
  getExtent = VS.length
instance Element a => Serialize     (VS.Vector a) where
  basicRead dset spc = do
    n <- dataspaceRank spc
    when (n /= Just 1) $ error "Invalid dimention"
    basicReadBuffer dset spc
  -- We don't have primitive we
  basicWrite dset xs = HIO $ do
    VS.unsafeWith xs $ \ptr -> unHIO $ do
      withType @a $ \ty ->
        unsafeWriteAll dset ty (castPtr ptr)


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
  type ExtentOf  (SerializeAsScalar a) = ()
  getExtent _ = ()

instance Element a => Serialize (SerializeAsScalar a) where
  basicRead  dset spc = basicReadScalar dset spc
  basicWrite dset a   = evalContT $ do
    p  <- ContT hioAlloca
    ty <- ContT $ withType @a
    lift $ do hioPoke p a
              unsafeWriteAll dset ty (castPtr p)

----------------------------------------------------------------
-- Primitive read/write operations
----------------------------------------------------------------

basicReadBuffer
  :: forall a d. (Element a, HasData d)
  => d
  -> Dataspace
  -> HIO (VS.Vector a)
basicReadBuffer dset (Dataspace spc) = do
  n   <- h5s_get_simple_extent_npoints spc
  buf <- hioMallocForeignPtrArray (fromIntegral n)
  evalContT $ do
    p  <- ContT $ hioWithForeignPtr buf
    ty <- ContT $ withType @a
    lift $ do unsafeReadAll dset ty (castPtr p)
              pure $! VS.unsafeFromForeignPtr0 buf (fromIntegral n)

basicReadScalar
  :: forall a d. (Element a, HasData d)
  => d
  -> Dataspace
  -> HIO a
basicReadScalar dset (Dataspace spc) = do
  nd <- h5s_get_simple_extent_ndims spc
  when (nd /= 0) $ throwM $ Error "Scalar could only be read from scalar datasets" []
  evalContT $ do
    p  <- ContT $ hioAlloca
    ty <- ContT $ withType @a
    lift $ do unsafeReadAll dset ty (castPtr p)
              hioPeek p
