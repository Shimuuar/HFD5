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
    -- ** Datasets
  , Dataset
  , openDataset
  , createEmptyDataset
  , createDataset
  , withOpenDataset
  , withCreateEmptyDataset
    -- ** Reading and writing
  , readDataset
  , readObject
  , readAt
    -- ** Dataspace information
  , Dataspace
  , Dim(..)
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
  , tyI8, tyI16, tyI32, tyI64
  , tyU8, tyU16, tyU32, tyU64
  , tyF32, tyF64
  , makeArray
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
  , SerializeDSet(..)
  , Serialize(..)
  , SerializeAttr(..)
    -- ** Primitives
  , basicReadBuffer
  , basicReadScalar
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import HDF5.HL.Internal            qualified as HIO
import HDF5.HL.Internal            ( Element(..), SerializeAttr(..), Serialize(..), SerializeDSet(..)
                                   , basicReadBuffer, basicReadScalar)
import HDF5.HL.Internal.TyHDF
import HDF5.HL.Internal.Types
import HDF5.HL.Internal.Error
import HDF5.HL.Internal.Dataspace
import HDF5.C
import Prelude hiding (read,readIO)


----------------------------------------------------------------
-- Lifted function from other modules
----------------------------------------------------------------

-- | Close value
close :: (Closable a, MonadIO m) => a -> m ()
close = unsafeRunHIO . basicClose

getType :: (HasData a, MonadIO m) => a -> m Type
getType = unsafeRunHIO . getTypeIO

getDataspace :: (HasData a, MonadIO m) => a -> m Dataspace
getDataspace = unsafeRunHIO . getDataspaceIO

----------------------------------------------------------------
-- File API
----------------------------------------------------------------

-- | Open HDF5 file. This function will throw exception when file
--   doesn't exists even if it's open for writing. Use 'createFile' to
--   create new file. Returned handle must be closed using 'close'.
openFile :: MonadIO m => FilePath -> OpenMode -> m File
openFile path mode = unsafeRunHIO $ HIO.openFile path mode

-- | Open file using 'openFile' and pass handle to continuation. It
--   will be closed when continuation finish execution normally or
--   abnormally.
withOpenFile
  :: (MonadMask m, MonadIO m)
  => FilePath -> OpenMode -> (File -> m a) -> m a
withOpenFile path mode = bracket (openFile path mode) close

-- | Create new HDF5 file or replace existing file. Use 'openFile' to
--   open existing file for modification. Returned handle must be
--   closed using 'close'.
createFile :: MonadIO m => FilePath -> CreateMode -> m File
createFile path mode = unsafeRunHIO $ HIO.createFile path mode

-- | Create file using 'createFile' and pass handle to
--   continuation. It will be closed when continuation finish
--   execution normally or abnormally.
withCreateFile
  :: (MonadMask m, MonadIO m)
  => FilePath -> CreateMode -> (File -> m a) -> m a
withCreateFile path mode = bracket (createFile path mode) close

----------------------------------------------------------------
-- Group API
----------------------------------------------------------------

openGroup
  :: (IsDirectory dir, MonadIO m)
  => dir      -- ^ Location
  -> FilePath -- ^ Name of group
  -> m Group
openGroup dir path = unsafeRunHIO $ HIO.openGroup dir path

withOpenGroup
  :: (IsDirectory dir, MonadIO m, MonadMask m)
  => dir              -- ^ Location
  -> FilePath         -- ^ Name of group
  -> (Group -> m a)
  -> m a
withOpenGroup dir path = bracket (openGroup dir path) close

createGroup
  :: (IsDirectory dir, MonadIO m)
  => dir       -- ^ Location
  -> FilePath  -- ^ Name of group
  -> m Group
createGroup dir path = unsafeRunHIO $ HIO.createGroup dir path

withCreateGroup
  :: (IsDirectory dir, MonadIO m, MonadMask m)
  => dir              -- ^ Location
  -> FilePath         -- ^ Name of group
  -> (Group -> m a)
  -> m a
withCreateGroup dir path = bracket (createGroup dir path) close


----------------------------------------------------------------
-- Dataset API
----------------------------------------------------------------

-- | Open existing dataset in given location. Returned 'Dataset' must
--   be closed by call to close.
openDataset
  :: (MonadIO m, IsDirectory dir)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> m Dataset
openDataset dir path = unsafeRunHIO $ HIO.openDataset dir path

-- | Create new dataset at given location without writing any data to
--   it. Returned 'Dataset' must be closed by call to 'close'.
createEmptyDataset
  :: (MonadIO m, IsDirectory dir, IsExtent ext)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> Type     -- ^ Element type
  -> ext      -- ^ Dataspace, that is size of dataset
  -> m Dataset
createEmptyDataset dir path ty ext
  = unsafeRunHIO $ HIO.createEmptyDataset dir path ty ext

-- | Create new dataset at given location and write provided data to
--   it. Shape of data is inferred from data to write.
createDataset
  :: forall a dir m.
     (SerializeDSet a, IsDirectory dir, MonadIO m)
  => dir      -- ^ File (root will be used) or group
  -> FilePath -- ^ Path to dataset
  -> a        -- ^ Value to write
  -> m ()
createDataset dir path a = unsafeRunHIO $ evalContT $ do
  ty   <- ContT $ HIO.withType @(ElementOf a)
  dset <- ContT $ HIO.withCreateEmptyDataset dir path ty (getExtent a)
  lift $ basicWriteDSet dset a


-- | Open dataset and pass handle to continuation. Dataset will be
--   closed when continuation finish execution normally or with an
--   exception.
withOpenDataset
  :: (MonadMask m, MonadIO m, IsDirectory dir)
  => dir      -- ^ Root
  -> FilePath -- ^ Path relative to root
  -> (Dataset -> m a)
  -> m a
withOpenDataset dir path = bracket (openDataset dir path) close

-- | Create new dataset at given location. Returned 'Dataset' must be
--   closed by call to 'close'.
withCreateEmptyDataset
  :: (MonadIO m, MonadMask m, IsDirectory dir, IsExtent ext)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> Type     -- ^ Element type
  -> ext      -- ^ Dataspace, that is size of dataset
  -> (Dataset -> m a)
  -> m a
withCreateEmptyDataset dir path ty ext = bracket
  (createEmptyDataset dir path ty ext)
  close

-- | Read data from already opened dataset. This function work
--   specifically with datasets and can use its attributes. Use 'read'
--   to be able to read from attributes as well.
readDataset :: (SerializeDSet a, MonadIO m) => Dataset -> m a
readDataset d = unsafeRunHIO $ HIO.readDataset d

-- | Read value from already opened dataset or attribute.
readObject :: (Serialize a, HasData d, MonadIO m) => d -> m a
readObject d = unsafeRunHIO $ bracket (getDataspaceIO d) basicClose (basicRead d)

-- | Open dataset and read it using 'readDSet'.
readAt
  :: (SerializeDSet a, IsDirectory dir, MonadIO m)
  => dir      -- ^ File (root will be used) or group
  -> FilePath -- ^ Path to dataset
  -> m a
readAt dir path = unsafeRunHIO $ HIO.withOpenDataset dir path HIO.readDataset


----------------------------------------------------------------
-- Dataspace API
----------------------------------------------------------------

rank :: (HasData a, MonadIO m) => a -> m (Maybe Int)
rank a = unsafeRunHIO $ withDataspace a HIO.dataspaceRank

-- | Compute extent of an object. Returns nothing when extent has
--   unexpected shape. E.g. if 2D array is expected but object is 1D
--   array.
extent :: (HasData a, IsExtent ext, MonadIO m) => a -> m (Maybe ext)
extent a = unsafeRunHIO $ withDataspace a runParseFromDataspace

dataspaceRank
  :: (MonadIO m)
  => Dataspace
  -> m (Maybe Int)
dataspaceRank spc = unsafeRunHIO $ HIO.dataspaceRank spc

-- | Parse extent of dataspace. Returns @Nothing@ if dataspace doens't
--   match expected shape.
dataspaceExt
  :: (MonadIO m, IsExtent ext)
  => Dataspace
  -> m (Maybe ext)
dataspaceExt spc = unsafeRunHIO $ runParseFromDataspace spc

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
openAttr a path = unsafeRunHIO $ HIO.openAttr a path

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
createAttr dir path a = unsafeRunHIO $ HIO.createAttr dir path a

readAttr
  :: (Serialize a, HasAttrs d, MonadIO m)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> m (Maybe a)
readAttr a name = unsafeRunHIO $ HIO.readAttr a name

