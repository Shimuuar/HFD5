{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Data types for working with HDF5 files
module HDF5.HL.Internal.Wrappers
  ( -- * Files and groups
    File(..)
  , OpenMode(..)
  , CreateMode(..)
  , Group(..)
  , Dataset(..)
  , Attribute(..)
  , Dataspace(..)
  , Type(..)
  , PropertyHID(..)
    -- * Type classes
  , Closable(..)
  , IsObject(..)
  , IsDirectory
  , HasAttrs
  , HasData(..)
  , withDataspace
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Foreign.Ptr
import Foreign.Marshal
import GHC.Stack

import HDF5.C
import HDF5.HL.Internal.Enum
import HDF5.HL.Internal.Error
import HDF5.HL.Internal.Types


----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Most value (files, groups, datasets, etc.) should be closed
--   explicitly in order to avoid resource leaks. This is utility
--   class which allows to use same function to all of them.
class Closable a where
  basicClose :: HasCallStack => a -> IO ()


-- | Some HDF5 object.
class IsObject a where
  getHID        :: a -> HID
  unsafeFromHID :: HID -> a

instance IsObject HID where
  getHID        = id
  unsafeFromHID = id


-- | HDF5 entities that could be used in context where group is
--   expected: groups, files (root group is used).
class IsObject a => IsDirectory a

-- | Objects which could have attributes attached, such as files and groups
class IsObject a => HasAttrs a

-- | HDF5 entities which contains data that could be
class IsObject a => HasData a where
  -- | Get type of object
  getTypeIO      :: HasCallStack => a -> IO Type
  -- | Get dataspace associated with object
  getDataspaceIO :: HasCallStack => a -> IO Dataspace
  -- | Read all content of object
  unsafeReadAll  :: HasCallStack
                 => a      -- ^ Object handle
                 -> Type   -- ^ Type of in-memory elements
                 -> Ptr x  -- ^ Buffer to read to
                 -> IO ()
  -- | Write full dataset at once
  unsafeWriteAll :: HasCallStack
                 => a      -- ^ Object handle
                 -> Type   -- ^ Type of in-memory elements
                 -> Ptr x  -- ^ Buffer with data
                 -> IO ()


withDataspace :: (HasData a) => a -> (Dataspace -> IO b) -> IO b
withDataspace a = bracket (getDataspaceIO a) basicClose


----------------------------------------------------------------
-- Files, groups, datasets
----------------------------------------------------------------

-- | Handle for working with HDF5 file.
newtype File = File HID
  deriving stock (Show,Eq,Ord)
  deriving newtype IsObject

-- | Handle for working with group (directory).
newtype Group = Group HID
  deriving stock (Show,Eq,Ord)
  deriving newtype IsObject

-- | Handle for dataset.
newtype Dataset = Dataset HID
  deriving stock (Show,Eq,Ord)
  deriving newtype IsObject

-- | Handle for attribute attached to file, group, directory.
newtype Attribute = Attribute HID
  deriving stock (Show,Eq,Ord)
  deriving newtype IsObject

-- | Handle for dataspace.
newtype Dataspace = Dataspace HID
  deriving stock (Show,Eq,Ord)
  deriving newtype IsObject

-- | Property list for values of type @p@.
newtype PropertyHID p = PropertyHID HID
  deriving stock (Show,Eq,Ord)
  deriving newtype IsObject

----------------

instance IsDirectory File
instance HasAttrs    File

----------------

instance IsDirectory Group
instance HasAttrs    Group

----------------

instance HasData Dataset where
  getTypeIO (Dataset hid) = withFrozenCallStack $ alloca $ \p_err ->
      unsafeNewType
    $ checkHID p_err "Cannot get type of dataset"
    $ h5d_get_type hid
  getDataspaceIO (Dataset hid) = withFrozenCallStack $ alloca $ \p_err ->
      fmap Dataspace
    $ checkHID p_err "Cannot read dataset's dataspace"
    $ h5d_get_space hid
  unsafeReadAll (Dataset hid) ty buf = evalContT $ withFrozenCallStack $ do
    p_err <- ContT $ alloca
    tid   <- ContT $ withType ty
    lift $ checkHErr p_err "Reading dataset data failed"
         $ h5d_read hid tid
             h5s_ALL h5s_ALL H5P_DEFAULT (castPtr buf)
  unsafeWriteAll (Dataset hid) ty buf = evalContT $ withFrozenCallStack $ do
    p_err <- ContT $ alloca
    tid   <- ContT $ withType ty
    lift $ checkHErr p_err "Writing dataset data failed"
         $ h5d_write hid tid
             h5s_ALL h5s_ALL H5P_DEFAULT buf

instance HasAttrs Dataset

----------------

instance HasData Attribute where
  getTypeIO (Attribute hid) = alloca $ \p_err -> withFrozenCallStack
    $ unsafeNewType
    $ checkHID p_err "Cannot get type of attribute"
    $ h5a_get_type hid
  getDataspaceIO (Attribute hid) = alloca $ \p_err -> withFrozenCallStack 
    $ fmap Dataspace
    $ checkHID p_err "Cannot get attribute's dataspace"
    $ h5a_get_space hid
  unsafeReadAll (Attribute hid) ty buf = evalContT $ withFrozenCallStack $ do
    p_err <- ContT $ alloca
    tid   <- ContT $ withType ty
    lift $ checkHErr p_err "Reading attribute data failed"
         $ h5a_read hid tid (castPtr buf)
  unsafeWriteAll (Attribute hid) ty buf = evalContT $ withFrozenCallStack $ do
    p_err <- ContT $ alloca
    tid   <- ContT $ withType ty
    lift $ checkHErr p_err "Writing Attribute data failed"
         $ h5a_write hid tid (castPtr buf)


----------------------------------------------------------------

instance Closable File where
  basicClose (File hid) =  alloca $ \p_err ->
      checkHErr p_err "Failed to close File"
    $ h5f_close hid

instance Closable Dataset where
  basicClose (Dataset hid) =  alloca $ \p_err ->
      checkHErr p_err "Failed to close Dataset"
    $ h5d_close hid

instance Closable Attribute where
  basicClose (Attribute hid) =  alloca $ \p_err ->
      checkHErr p_err "Failed to close Attribute"
    $ h5a_close hid

instance Closable Dataspace where
  basicClose (Dataspace hid) = alloca $ \p_err ->
      checkHErr p_err "Failed to close Dataspace"
    $ h5s_close hid

instance Closable Group where
  basicClose (Group hid) = alloca $ \p_err ->
      checkHErr p_err "Failed to close Group"
    $ h5g_close hid

instance Closable (PropertyHID p) where
  basicClose (PropertyHID hid) = alloca $ \p_err ->
      checkHErr p_err "Failed to close PropertyHID"
    $ h5p_close hid

