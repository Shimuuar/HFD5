{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Data types for working with HDF5 files
module HDF5.HL.Internal.Types
  ( -- * Files and groups
    File(..)
  , OpenMode(..)
  , CreateMode(..)
  , Group(..)
  , Dataset(..)
  , Attribute(..)
  , Dataspace(..)
  , Type(..)
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
import Data.Coerce
import Foreign.Ptr
import Foreign.Marshal
import HDF5.C
import HDF5.HL.Internal.Enum
import HDF5.HL.Internal.Error
import HDF5.HL.Internal.TyHDF


----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Most value (files, groups, datasets, etc.) should be closed
--   explicitly in order to avoid resource leaks. This is utility
--   class which allows to use same function to all of them.
class Closable a where
  basicClose :: a -> IO ()


-- | Some HDF5 object.
class IsObject a where
  getHID        :: a -> HID
  unsafeFromHID :: HID -> a


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
                 -> Ptr x  -- ^ Buffer to read to
                 -> IO ()
  -- | Write full dataset at once
  unsafeWriteAll :: a      -- ^ Object handle
                 -> Type   -- ^ Type of in-memory elements
                 -> Ptr x  -- ^ Buffer with data
                 -> IO ()


withDataspace :: (HasData a) => a -> (Dataspace -> IO b) -> IO b
withDataspace a = bracket (getDataspaceIO a) basicClose


----------------------------------------------------------------
-- Files, groups, datasets
----------------------------------------------------------------

-- | Handle for working with HDF5 file
newtype File = File HID
  deriving stock (Show,Eq,Ord)

-- | Handle for working with group (directory)
newtype Group = Group HID
  deriving stock (Show,Eq,Ord)

-- | Handle for dataset
newtype Dataset = Dataset HID
  deriving stock (Show,Eq,Ord)

-- | Handle for attribute
newtype Attribute = Attribute HID
  deriving stock (Show,Eq,Ord)

-- | Handle for dataspace
newtype Dataspace = Dataspace HID
  deriving stock (Show,Eq,Ord)


----------------

instance IsObject File where
  getHID        = coerce
  unsafeFromHID = coerce

instance IsDirectory File
instance HasAttrs    File

----------------

instance IsObject Group where
  getHID        = coerce
  unsafeFromHID = coerce

instance IsDirectory Group
instance HasAttrs    Group

----------------

instance IsObject Dataset where
  getHID        = coerce
  unsafeFromHID = coerce

instance HasData Dataset where
  getTypeIO (Dataset hid) = alloca $ \p_err ->
      unsafeNewType
    $ checkHID p_err (makeMessage "getTypeIO @Dataset" "Cannot get type of dataset")
    $ h5d_get_type hid
  getDataspaceIO (Dataset hid) = alloca $ \p_err ->
      fmap Dataspace
    $ checkHID p_err (makeMessage "getDataspaceIO @Dataset" "Cannot read dataset's dataspace")
    $ h5d_get_space hid
  unsafeReadAll (Dataset hid) ty buf = evalContT $ do
    p_err <- ContT $ alloca
    tid   <- ContT $ withType ty
    lift $ checkHErr p_err (makeMessage "unsafeReadAll @Dataset" "Reading dataset data failed")
         $ h5d_read hid tid
             h5s_ALL h5s_ALL h5p_DEFAULT (castPtr buf)
  unsafeWriteAll (Dataset hid) ty buf = evalContT $ do
    p_err <- ContT $ alloca
    tid   <- ContT $ withType ty
    lift $ checkHErr p_err (makeMessage "unsafeWriteAll @Dataset" "Writing dataset data failed")
         $ h5d_write hid tid
             h5s_ALL h5s_ALL h5p_DEFAULT buf

instance HasAttrs Dataset

----------------

instance IsObject Attribute where
  getHID        = coerce
  unsafeFromHID = coerce

instance HasData Attribute where
  getTypeIO (Attribute hid) = alloca $ \p_err -> 
      unsafeNewType
    $ checkHID p_err (makeMessage "getTypeIO @Attribute" "Cannot get type of attribute")
    $ h5a_get_type hid
  getDataspaceIO (Attribute hid) = alloca $ \p_err ->
      fmap Dataspace
    $ checkHID p_err (makeMessage "getDataspaceIO @Attribute" "Cannot get attribute's dataspace")
    $ h5a_get_space hid
  unsafeReadAll (Attribute hid) ty buf = evalContT $ do
    p_err <- ContT $ alloca
    tid   <- ContT $ withType ty
    lift $ checkHErr p_err (makeMessage "unsafeReadAll @Attribute" "Reading attribute data failed")
         $ h5a_read hid tid (castPtr buf)
  unsafeWriteAll (Attribute hid) ty buf = evalContT $ do
    p_err <- ContT $ alloca
    tid   <- ContT $ withType ty
    lift $ checkHErr p_err (makeMessage "unsafeWriteAll @Attribute" "Writing attribute data failed")
         $ h5a_write hid tid (castPtr buf)

----------------

instance IsObject Dataspace where
  getHID        = coerce
  unsafeFromHID = coerce


----------------------------------------------------------------

instance Closable File where
  basicClose (File hid) =  alloca $ \p_err ->
      checkHErr p_err (makeMessage "basicClose @File" "Failed to close")
    $ h5f_close hid

instance Closable Dataset where
  basicClose (Dataset hid) =  alloca $ \p_err ->
      checkHErr p_err (makeMessage "basicClose @Dataset" "Failed to close")
    $ h5d_close hid

instance Closable Attribute where
  basicClose (Attribute hid) =  alloca $ \p_err ->
      checkHErr p_err (makeMessage "basicClose @Dataspace" "Failed to close")
    $ h5a_close hid

instance Closable Dataspace where
  basicClose (Dataspace hid) = alloca $ \p_err ->
      checkHErr p_err (makeMessage "basicClose @Dataspace" "Failed to close")
    $ h5s_close hid

instance Closable Group where
  basicClose (Group hid) = alloca $ \p_err ->
      checkHErr p_err (makeMessage "basicClose @Group" "Failed to close")
    $ h5g_close hid

makeMessage :: String -> String -> MessageHS
makeMessage func descr = MessageHS
  { msgHsDescr = descr
  , msgHsFile  = "HDF5.HL.Internal.Types"
  , msgHsFunc  = func
  }
