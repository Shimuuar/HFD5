{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Data types for working with HDF5 files
module HDF5.HL.Internal.Types
  ( -- * Files and groups
    File(..)
  , OpenMode(..)
  , CreateMode(..)
  , Dataset(..)
  , Attribute(..)
  , Dataspace(..)
    -- * Exceptions
  , HDF5Error(..)
    -- * Type classes
  , Closable(..)
  , close
  , IsObject(..)
  , castObj
  , castObj'
  , IsDirectory
  , HasAttrs
  , HasData(..)
  , getType
  , getDataspace
  , withDataspace
  ) where

import Control.Exception      (throw)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Coerce
import Foreign.Ptr
import HDF5.C qualified as C
import HDF5.HL.Internal.CCall
import HDF5.HL.Internal.Enum
import HDF5.HL.Internal.TyHDF


----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Most value (files, groups, datasets, etc.) should be closed
--   explicitly in order to avoid resource leaks. This is utility
--   class which allows to use same function to all of them.
class Closable a where
  closeIO :: a -> IO ()

-- | Lifted variant of 'closeIO'
close :: (Closable a, MonadIO m) => a -> m ()
close = liftIO . closeIO



-- | Some HDF5 object. 
class IsObject a where
  getHID        :: a -> C.HID
  unsafeFromHID :: C.HID -> a
  getTag        :: ObjTag

-- | Cast one object to another object
castObj :: forall a b. (IsObject a, IsObject b) => a -> Maybe b
castObj a | getTag @a == getTag @b = Just $! unsafeFromHID $ getHID a
          | otherwise              = Nothing

-- | Cast one object to another object. In case of failure throw
--   exception 'CastError'.
castObj' :: forall a b. (IsObject a, IsObject b) => a -> b
castObj' a | getTag @a == getTag @b = unsafeFromHID $ getHID a
          | otherwise               = throw $ CastError (getTag @a) (getTag @b)

  
-- | HDF5 entities that could be used in context where group is
--   expected: groups, files (root group is used).
class IsObject a => IsDirectory a

-- | Objects which could have attributes attached, such as files and groups
class IsObject a => HasAttrs a
  
-- | HDF5 entities which contains data that could be 
class IsObject a => HasData a where
  -- | Get type of object
  getTypeIO      :: a -> IO Type
  -- | Get dataspace associated with object
  getDataspaceIO :: a -> IO Dataspace
  -- | Read all content of object
  unsafeReadAll  :: a      -- ^ Object handle
                 -> Type   -- ^ Type of in-memory elements 
                 -> Ptr () -- ^ Buffer to read to
                 -> IO ()
  -- | Write full dataset at once
  unsafeWriteAll :: a      -- ^ Object handle
                 -> Type   -- ^ Type of in-memory elements
                 -> Ptr () -- ^ Buffer with data
                 -> IO ()
                 
  
getType :: (HasData a, MonadIO m) => a -> m Type
getType = liftIO . getTypeIO

getDataspace :: (HasData a, MonadIO m) => a -> m Dataspace
getDataspace = liftIO . getDataspaceIO

withDataspace :: (HasData a, MonadIO m, MonadMask m) => a -> (Dataspace -> m b) -> m b
withDataspace a = bracket (getDataspace a) close


----------------------------------------------------------------
-- Files, groups, datasets
----------------------------------------------------------------

-- | Handle for working with HDF5 file
newtype File = File C.HID
  deriving stock (Show,Eq,Ord)

-- | Handle for working with group (directory)
newtype Group = Group C.HID
  deriving stock (Show,Eq,Ord)

-- | Handle for dataset
newtype Dataset = Dataset C.HID
  deriving stock (Show,Eq,Ord)

-- | Handle for attribute
newtype Attribute = Attribute C.HID
  deriving stock (Show,Eq,Ord)

-- | Handle for dataspace
newtype Dataspace = Dataspace C.HID
  deriving stock (Show,Eq,Ord)



instance IsObject File where
  getHID        = coerce
  unsafeFromHID = coerce
  getTag        = TagFile

instance IsDirectory File

----------------

instance IsObject Group where
  getHID        = coerce
  unsafeFromHID = coerce
  getTag        = TagGroup

instance IsDirectory Group
instance HasAttrs    Group

----------------

instance IsObject Dataset where
  getHID        = coerce
  unsafeFromHID = coerce
  getTag        = TagDataset

instance HasData Dataset where
  getTypeIO (Dataset hid) = unsafeNewType $ do
    checkINV "Cannot read type from dataset" =<< C.h5d_get_type hid
  getDataspaceIO (Dataset hid) = Dataspace <$> do
    checkINV "Cannot read dataspace from dataset" =<< C.h5d_get_space hid
  unsafeReadAll (Dataset hid) ty buf = withType ty $ \tid ->
    convertHErr "Reading from dataset failed" $ C.h5d_read hid tid
      C.h5s_ALL
      C.h5s_ALL
      C.h5p_DEFAULT
      (castPtr buf)
  unsafeWriteAll (Dataset hid) ty buf = withType ty $ \tid ->
    convertHErr "Reading to dataset failed" $ C.h5d_write hid tid
      C.h5s_ALL
      C.h5s_ALL
      C.h5p_DEFAULT
      buf

instance HasAttrs Dataset

----------------

instance IsObject Attribute where
  getHID        = coerce
  unsafeFromHID = coerce
  getTag        = TagAttribute

instance HasData Attribute where
  getTypeIO (Attribute hid) = unsafeNewType $ do
    checkINV "Cannot read type from attribute" =<< C.h5a_get_type hid
  getDataspaceIO (Attribute hid) = Dataspace <$> do
    checkINV "Cannot read dataspace from dataset" =<< C.h5a_get_space hid
  unsafeReadAll (Attribute hid) ty buf = withType ty $ \tid ->
    convertHErr "Reading from attribute failed" $
      C.h5a_read hid tid (castPtr buf)
  unsafeWriteAll (Attribute hid) ty buf = withType ty $ \tid ->
    convertHErr "Writing of attribute failed" $
      C.h5a_write hid tid (castPtr buf)

----------------

instance IsObject Dataspace where
  getHID        = coerce
  unsafeFromHID = coerce
  getTag        = TagDataspace



instance Closable File where
  closeIO (File hid) = convertHErr "Unable to close file" $ C.h5f_close hid
instance Closable Dataset where
  closeIO (Dataset hid) = convertHErr "Unable to close dataset" $ C.h5d_close hid
instance Closable Attribute where
  closeIO (Attribute hid) = convertHErr "Unable to close attribute" $ C.h5a_close hid
instance Closable Dataspace where
  closeIO (Dataspace hid) = convertHErr "Unable to close dataspace" $ C.h5s_close hid


-- instance Closable Group where
--   closeIO (Group hid) = convertHErr "Unable to close group" $ C.h5g_close hid

