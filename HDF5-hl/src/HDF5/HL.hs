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
  ( -- * High level API
    -- ** File operations
    File
  , OpenMode(..)
  , openFile
  , withOpenFile
  , CreateMode(..)
  , createFile
  , withCreateFile
    -- * Datasets
  , Dataset
  , openDataset
  , createEmptyDataset
  , createDataset
  , withOpenDataset
  , withCreateEmptyDataset
    -- ** Reading and writing
  , Element(..)
  , SerializeDSet(..)
  , Serialize(..)
  , readDataset
  , readObject
  , readAt
  , SerializeAttr(..)
    -- ** Dataspace information
  , Dataspace
  , Dim(..)
  , Extent(..)
  , rank
  , extent
  , dataspaceRank
  , dataspaceExt
    -- * Attributes
  , Attribute
  , openAttr
  , withAttr
  , createAttr
  , readAttr
    -- * Low-level API
  , basicReadBuffer
  , basicReadScalar
    -- * Data types
  , Type
  , tyI8, tyI16, tyI32, tyI64
  , tyU8, tyU16, tyU32, tyU64
  , tyF32, tyF64
  , pattern Array
    -- * Error handling
  , Error(..)
    -- * Type classes
    -- ** HDF objects
  , IsObject
  , IsDirectory
  , HasData(..)
  , getType
  , getDataspace
  , HasAttrs
    -- ** Closing objects
  , Closable
  , close
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
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
close = liftIO . runHIO . basicClose


getType :: (HasData a, MonadIO m) => a -> m Type
getType = liftIO . runHIO . getTypeIO

getDataspace :: (HasData a, MonadIO m) => a -> m Dataspace
getDataspace = liftIO . runHIO . getDataspaceIO

----------------------------------------------------------------
-- File API
----------------------------------------------------------------

-- | Open HDF5 file. This function will throw exception when file
--   doesn't exists even if it's open for writing. Use 'createFile' to
--   create new file. Returned handle must be closed using 'close'.
openFile :: MonadIO m => FilePath -> OpenMode -> m File
openFile path mode = liftIO . runHIO $ HIO.openFile path mode

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
createFile path mode = liftIO $ runHIO $ HIO.createFile path mode

-- | Create file using 'createFile' and pass handle to
--   continuation. It will be closed when continuation finish
--   execution normally or abnormally.
withCreateFile
  :: (MonadMask m, MonadIO m)
  => FilePath -> CreateMode -> (File -> m a) -> m a
withCreateFile path mode = bracket (createFile path mode) close


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
openDataset dir path = liftIO $ runHIO $ HIO.openDataset dir path

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
  = liftIO $ runHIO $ HIO.createEmptyDataset dir path ty ext

-- | Create new dataset at given location and write provided data to
--   it. Shape of data is inferred from data to write.
createDataset
  :: forall a dir m.
     (SerializeDSet a, IsDirectory dir, MonadIO m)
  => dir      -- ^ File (root will be used) or group
  -> FilePath -- ^ Path to dataset
  -> a        -- ^ Value to write
  -> m ()
createDataset dir path a
  = liftIO . runHIO
  $ HIO.withCreateEmptyDataset dir path (typeH5 @(ElementOf a)) (getExtent a)
  $ \dset -> basicWriteDSet dset a


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
readDataset d = liftIO $ runHIO $ HIO.readDataset d

-- | Read value from already opened dataset or attribute.
readObject :: (Serialize a, HasData d, MonadIO m) => d -> m a
readObject d = liftIO $ runHIO $ bracket (getDataspaceIO d) basicClose (basicRead d)

-- | Open dataset and read it using 'readDSet'.
readAt
  :: (SerializeDSet a, IsDirectory dir, MonadIO m)
  => dir      -- ^ File (root will be used) or group
  -> FilePath -- ^ Path to dataset
  -> m a
readAt dir path = liftIO . runHIO $ HIO.withOpenDataset dir path HIO.readDataset


----------------------------------------------------------------
-- Dataspace API
----------------------------------------------------------------

rank :: (HasData a, MonadIO m) => a -> m (Maybe Int)
rank a = liftIO $ runHIO $ withDataspace a HIO.dataspaceRank

-- | Compute extent of an object. Returns nothing when extent has
--   unexpected shape. E.g. if 2D array is expected but object is 1D
--   array.
extent :: (HasData a, IsExtent ext, MonadIO m) => a -> m (Maybe ext)
extent a = liftIO $ runHIO $ withDataspace a runParseFromDataspace

dataspaceRank
  :: (MonadIO m)
  => Dataspace
  -> m (Maybe Int)
dataspaceRank spc = liftIO $ runHIO $ HIO.dataspaceRank spc

-- | Parse extent of dataspace. Returns @Nothing@ if dataspace doens't
--   match expected shape.
dataspaceExt
  :: (MonadIO m, IsExtent ext)
  => Dataspace
  -> m (Maybe ext)
dataspaceExt spc = liftIO $ runHIO $ runParseFromDataspace spc

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
openAttr a path = liftIO $ runHIO $ HIO.openAttr a path

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
createAttr dir path a = liftIO $ runHIO $ HIO.createAttr dir path a

readAttr
  :: (Serialize a, HasAttrs d, MonadIO m)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> m (Maybe a)
readAttr a name = liftIO $ runHIO $ HIO.withAttr a name $ \case
  Just x  -> Just <$> HIO.readObject x
  Nothing -> pure Nothing
