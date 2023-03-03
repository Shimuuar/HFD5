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
  ( -- * Data types and common operations
    -- ** File operations
    File
  , OpenMode(..)
  , openFile
  , withOpenFile
  , CreateMode(..)
  , createFile
  , withCreateFile
    -- ** Group operation
  , Group
  , openGroup
  , createGroup
  , withOpenGroup
  , withCreateGroup
  , listGroup
    -- ** Datasets
  , Dataset
  , openDataset
  , createEmptyDataset
  , createDataset
  , withOpenDataset
  , withCreateEmptyDataset
  , withCreateDataset
  , setDatasetExtent
    -- ** Reading and writing
  , readDataset
  , readObject
  , readAt
  , readSlab
  , writeSlab
    -- ** Dataspace information
  , Dataspace
  , pattern UNLIMITED
  , Extent(..)
  , rank
  , extent
  , dataspaceRank
  , dataspaceExt
    -- ** Attributes
  , Attribute
  , openAttr
  , withAttr
  , createAttr
  , readAttr
    -- ** Data types
  , Type
  , sizeOfH5
  , tyI8, tyI16, tyI32, tyI64
  , tyU8, tyU16, tyU32, tyU64
  , tyF32, tyF64
  , pattern Array
  , makePackedRecord
  , makeEnumeration
    -- ** Property lists
  , Property
  , Layout(..)
  , propDatasetLayout
    -- ** Type classes
  , IsObject
  , IsDirectory
  , HasData(..)
  , getType
  , getDataspace
  , HasAttrs
  , Closable
  , close
    -- * Error handling
  , Error(..)
    -- * Serialization of haskell value
    -- ** Type classes
  , Element(..)
  , Serialize(..)
  , SerializeArr(..)
  , SerializeSlab(..)
    -- ** Primitives
  , basicReadBuffer
  , basicReadScalar
  , HIO.basicReadAttr
  , HIO.basicCreateAttr
    -- * Attributes
  , SerializeAttr(..)
  , HIO.AttributeM(..)
  , HIO.runAttributeM
  , HIO.basicAttrSubset
  , HIO.basicEncodeAttr
  , HIO.basicDecodeAttr
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.IORef
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack

import HDF5.HL.Internal            qualified as HIO
import HDF5.HL.Internal            ( SerializeAttr(..), Serialize(..), SerializeArr(..), SerializeSlab(..)
                                   , basicReadBuffer, basicReadScalar)
import HDF5.HL.Internal.Types
import HDF5.HL.Internal.Wrappers
import HDF5.HL.Internal.Error
import HDF5.HL.Internal.Enum
import HDF5.HL.Internal.Dataspace
import HDF5.HL.Internal.Property
import HDF5.C
import Prelude hiding (read,readIO)


----------------------------------------------------------------
-- Lifted function from other modules
----------------------------------------------------------------

-- | Close value
close :: (Closable a, MonadIO m, HasCallStack) => a -> m ()
close = liftIO . basicClose

getType :: (HasData a, MonadIO m, HasCallStack) => a -> m Type
getType = liftIO . getTypeIO

getDataspace :: (HasData a, MonadIO m, HasCallStack) => a -> m Dataspace
getDataspace = liftIO . getDataspaceIO

----------------------------------------------------------------
-- File API
----------------------------------------------------------------

-- | Open HDF5 file. This function will throw exception when file
--   doesn't exists even if it's open for writing. Use 'createFile' to
--   create new file. Returned handle must be closed using 'close'.
openFile :: (MonadIO m, HasCallStack) => FilePath -> OpenMode -> m File
openFile path mode = liftIO $ withFrozenCallStack $ evalContT $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  lift $ fmap File
       $ checkHID p_err ("Cannot open file " ++ path)
       $ h5f_open c_path (toCParam mode) H5P_DEFAULT

-- | Open file using 'openFile' and pass handle to continuation. It
--   will be closed when continuation finish execution normally or
--   abnormally.
withOpenFile
  :: (MonadMask m, MonadIO m, HasCallStack)
  => FilePath -> OpenMode -> (File -> m a) -> m a
withOpenFile path mode = bracket (openFile path mode) close

-- | Create new HDF5 file or replace existing file. Use 'openFile' to
--   open existing file for modification. Returned handle must be
--   closed using 'close'.
createFile :: (MonadIO m, HasCallStack) => FilePath -> CreateMode -> m File
createFile path mode = liftIO $ withFrozenCallStack $ evalContT $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  lift $ fmap File
       $ checkHID p_err ("Cannot create file " ++ path)
       $ h5f_create c_path (toCParam mode) H5P_DEFAULT H5P_DEFAULT

-- | Create file using 'createFile' and pass handle to
--   continuation. It will be closed when continuation finish
--   execution normally or abnormally.
withCreateFile
  :: (MonadMask m, MonadIO m, HasCallStack)
  => FilePath -> CreateMode -> (File -> m a) -> m a
withCreateFile path mode = bracket (createFile path mode) close

----------------------------------------------------------------
-- Group API
----------------------------------------------------------------

openGroup
  :: (IsDirectory dir, MonadIO m, HasCallStack)
  => dir      -- ^ Location
  -> FilePath -- ^ Name of group
  -> m Group
openGroup dir path = liftIO $ withFrozenCallStack $ evalContT $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  lift $ fmap Group
       $ checkHID p_err ("Cannot open group " ++ path)
       $ h5g_open (getHID dir) c_path H5P_DEFAULT

withOpenGroup
  :: (IsDirectory dir, MonadIO m, MonadMask m, HasCallStack)
  => dir              -- ^ Location
  -> FilePath         -- ^ Name of group
  -> (Group -> m a)
  -> m a
withOpenGroup dir path = bracket (openGroup dir path) close

createGroup
  :: (IsDirectory dir, MonadIO m, HasCallStack)
  => dir       -- ^ Location
  -> FilePath  -- ^ Name of group
  -> m Group
createGroup dir path = liftIO $ withFrozenCallStack $ evalContT $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  lift $ fmap Group
       $ checkHID p_err ("Cannot create group " ++ path)
       $ h5g_create (getHID dir) c_path H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT

withCreateGroup
  :: (IsDirectory dir, MonadIO m, MonadMask m, HasCallStack)
  => dir              -- ^ Location
  -> FilePath         -- ^ Name of group
  -> (Group -> m a)
  -> m a
withCreateGroup dir path = bracket (createGroup dir path) close

-- | List all names in the group
listGroup
  :: (IsDirectory dir, MonadIO m, HasCallStack)
  => dir -- ^ Location to use
  -> m [FilePath]
listGroup dir = liftIO $ withFrozenCallStack $ evalContT $ do
  p_err <- ContT $ alloca
  p_idx <- ContT $ alloca
  names <- lift  $ newIORef []
  let readNode _hid cname _p_node _p_userdata = do
        name <- peekCString cname
        modifyIORef' names (name:)
        pure $ HErr 0
  callback <- ContT $ bracket (makeH5LIterate2 readNode) freeHaskellFunPtr
  lift $ do
    poke p_idx 0 -- We MUST set starting index
    checkHErr p_err "Unable to iterate over group"
       $ h5l_iterate (getHID dir) H5_INDEX_NAME H5_ITER_DEC p_idx callback nullPtr
    readIORef names


----------------------------------------------------------------
-- Dataset API
----------------------------------------------------------------

-- | Open existing dataset in given location. Returned 'Dataset' must
--   be closed by call to close.
openDataset
  :: (MonadIO m, IsDirectory dir, HasCallStack)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> m Dataset
openDataset dir path = liftIO $ withFrozenCallStack $ evalContT $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  lift $  Dataset
      <$> ( checkHID p_err ("Cannot open dataset " ++ path)
          $ h5d_open2 (getHID dir) c_path H5P_DEFAULT)

-- | Create new dataset at given location without writing any data to
--   it. Returned 'Dataset' must be closed by call to 'close'.
createEmptyDataset
  :: (MonadIO m, IsDirectory dir, IsExtent ext, HasCallStack)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> Type     -- ^ Element type
  -> ext      -- ^ Dataspace, that is size of dataset
  -> [Property Dataset]
  -> m Dataset
createEmptyDataset dir path ty ext props = liftIO $ evalContT $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  space  <- ContT $ withCreateDataspace ext Nothing
  tid    <- ContT $ withType ty
  plist  <- ContT $ withDatasetProps $ mconcat props
  lift $ withFrozenCallStack
       $ fmap Dataset
       $ checkHID p_err ("Unable to create dataset")
       $ h5d_create (getHID dir) c_path tid (getHID space)
         H5P_DEFAULT
         (getHID plist)
         H5P_DEFAULT


-- | Create new dataset at given location and write provided data to
--   it. Shape of data is inferred from data to write.
createDataset
  :: forall a dir m. (Serialize a, IsDirectory dir, MonadIO m, HasCallStack)
  => dir      -- ^ File (root will be used) or group
  -> FilePath -- ^ Path to dataset
  -> a        -- ^ Value to write
  -> m ()
createDataset dir path a = liftIO $ evalContT $ do
  dset <- ContT $ withCreateEmptyDataset dir path (typeH5 @(ElementOf a)) (getExtent a)
  lift $ basicWrite dset a


-- | Open dataset and pass handle to continuation. Dataset will be
--   closed when continuation finish execution normally or with an
--   exception.
withOpenDataset
  :: (MonadMask m, MonadIO m, IsDirectory dir, HasCallStack)
  => dir      -- ^ Root
  -> FilePath -- ^ Path relative to root
  -> (Dataset -> m a)
  -> m a
withOpenDataset dir path = bracket (openDataset dir path) close

-- | Create new dataset at given location. Returned 'Dataset' must be
--   closed by call to 'close'.
withCreateEmptyDataset
  :: (MonadIO m, MonadMask m, IsDirectory dir, IsExtent ext, HasCallStack)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> Type     -- ^ Element type
  -> ext      -- ^ Dataspace, that is size of dataset
  -> (Dataset -> m a)
  -> m a
withCreateEmptyDataset dir path ty ext = bracket
  (createEmptyDataset dir path ty ext [])
  close

-- | Create new dataset at given location and populate it using data
--   from provided data structure.
withCreateDataset
  :: forall a m r dir. (MonadIO m, MonadMask m, IsDirectory dir, Serialize a, HasCallStack)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> a        -- ^ Data to write
  -> (Dataset -> m r)
  -> m r
withCreateDataset dir path a action = evalContT $ do
  dset <- ContT $ withCreateEmptyDataset dir path (typeH5 @(ElementOf a)) (getExtent a)
  liftIO $ basicWrite dset a
  lift   $ action dset

-- | Read data from already opened dataset. This function work
--   specifically with datasets and can use its attributes. Use 'read'
--   to be able to read from attributes as well.
readDataset :: (Serialize a, MonadIO m, HasCallStack) => Dataset -> m a
readDataset d = liftIO $ withDataspace d $ \spc -> basicRead d spc

-- | Read value from already opened dataset or attribute.
readObject :: (SerializeArr a, HasData d, MonadIO m, HasCallStack) => d -> m a
readObject = liftIO . HIO.basicReadObject

-- | Open dataset and read it using 'readDSet'.
readAt
  :: (Serialize a, IsDirectory dir, MonadIO m, HasCallStack)
  => dir      -- ^ File (root will be used) or group
  -> FilePath -- ^ Path to dataset
  -> m a
readAt dir path = liftIO $ withOpenDataset dir path readDataset

-- | Read slab selection from dataset
readSlab
  :: (SerializeSlab a, MonadIO m, HasCallStack)
  => Dataset    -- ^ Dataset to read from
  -> ExtentOf a -- ^ Offset into dataset
  -> ExtentOf a -- ^ Size to read
  -> m a
readSlab dset off sz = liftIO $ basicReadSlab dset off sz

-- | Write provided data into slab selection
writeSlab
  :: (SerializeSlab a, MonadIO m, HasCallStack)
  => Dataset    -- ^ Dataset to read from
  -> ExtentOf a -- ^ Offset into dataset
  -> a          -- ^ Data to write (will write all)
  -> m ()
writeSlab dset off xs = liftIO $ basicWriteSlab dset off xs

-- | Set new extent of dataspace. This function could be applied to
--   following datasets:
--
--   * Chunked dataset with unlimited dimensions
--
--   * A chunked dataset with fixed dimensions if the new dimension
--     sizes are less than the maximum sizes set with maxdims
setDatasetExtent :: (HasCallStack, IsExtent dim, MonadIO m) => Dataset -> dim -> m ()
setDatasetExtent dset dim = liftIO $ evalContT $ do
  p_err         <- ContT $ alloca
  (r_ext,p_ext) <- withEncodedExtent dim >>= \case
    Nothing -> throwM $ Error [Left "Extent must be non-null"]
    Just x  -> pure x
  r_dset <- lift
          $ checkCInt p_err "Cannot get rank of dataspace's extent"
          $ h5s_get_simple_extent_ndims (getHID dset)
  when (fromIntegral r_ext /= r_dset) $ throwM $
    Error [Left "Rank of dataset and rank of new extent do not match"]
  lift $ checkHErr p_err "Failed to set new extent for a dataset"
       $ h5d_set_extent (getHID dset) p_ext




----------------------------------------------------------------
-- Dataspace API
----------------------------------------------------------------

rank :: (HasData a, MonadIO m, HasCallStack) => a -> m (Maybe Int)
rank a = liftIO $ withDataspace a HIO.dataspaceRank

-- | Compute extent of an object. Returns nothing when extent has
--   unexpected shape. E.g. if 2D array is expected but object is 1D
--   array.
extent :: (HasData a, IsExtent ext, MonadIO m, HasCallStack) => a -> m (Maybe ext)
extent a = liftIO $ fmap fst <$> withDataspace a runParseFromDataspace

dataspaceRank
  :: (MonadIO m, HasCallStack)
  => Dataspace
  -> m (Maybe Int)
dataspaceRank spc = liftIO $ HIO.dataspaceRank spc

-- | Parse extent of dataspace. Returns @Nothing@ if dataspace doens't
--   match expected shape.
dataspaceExt
  :: (MonadIO m, IsExtent ext, HasCallStack)
  => Dataspace
  -> m (Maybe ext)
dataspaceExt spc = liftIO $ fmap fst <$> runParseFromDataspace spc

----------------------------------------------------------------
-- Attributes
----------------------------------------------------------------

-- | Open attribute of object. It could be either dataset or
--   group. Returns @Nothing@ if such attribute does not exists
openAttr
  :: (MonadIO m, HasAttrs a, HasCallStack)
  => a      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> m (Maybe Attribute)
openAttr a path = liftIO $ HIO.openAttr a path

-- | Open attribute of given group or dataset and pass handle to
--   continuation. It'll be closed when continuation finish
--   execution normally or with an exception.
withAttr
  :: (MonadMask m, MonadIO m, HasAttrs a, HasCallStack)
  => a      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> (Maybe Attribute -> m b)
  -> m b
withAttr a path = bracket (openAttr a path) (mapM_ close)

-- | Create attribute
createAttr
  :: forall a dir m. (SerializeArr a, HasAttrs dir, MonadIO m, HasCallStack)
  => dir    -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> a      -- ^ Value to store in attribute
  -> m ()
createAttr dir path a = liftIO $ HIO.basicCreateAttr dir path a

readAttr
  :: (SerializeArr a, HasAttrs d, MonadIO m, HasCallStack)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> m (Maybe a)
readAttr a name = liftIO $ HIO.basicReadAttr a name
