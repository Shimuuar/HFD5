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
    -- * Type classes
  , Closable(..)
  , IsObject(..)
  , castObj
  , castObj'
  , IsDirectory
  , HasAttrs
  , HasData(..)
  , withDataspace
  ) where

import Control.Monad.Catch
import Data.Coerce
import Foreign.Ptr
import HDF5.C
import HDF5.HL.Internal.CCall
import HDF5.HL.Internal.Enum
import HDF5.HL.Internal.TyHDF
import HDF5.HL.Internal.Error

----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Most value (files, groups, datasets, etc.) should be closed
--   explicitly in order to avoid resource leaks. This is utility
--   class which allows to use same function to all of them.
class Closable a where
  basicClose :: a -> HIO ()



-- | Some HDF5 object. 
class IsObject a where
  getHID        :: a -> HID
  unsafeFromHID :: HID -> a
  getTag        :: ObjTag

-- | Cast one object to another object
castObj :: forall a b. (IsObject a, IsObject b) => a -> Maybe b
castObj a | getTag @a == getTag @b = Just $! unsafeFromHID $ getHID a
          | otherwise              = Nothing

-- | Cast one object to another object. In case of failure throw
--   exception 'CastError'.
castObj' :: forall a b. (IsObject a, IsObject b) => a -> b
castObj' a
  -- FIXME: do we need this function???
  | getTag @a == getTag @b = unsafeFromHID $ getHID a
  | otherwise              = error "castObj' failed"

  
-- | HDF5 entities that could be used in context where group is
--   expected: groups, files (root group is used).
class IsObject a => IsDirectory a

-- | Objects which could have attributes attached, such as files and groups
class IsObject a => HasAttrs a
  
-- | HDF5 entities which contains data that could be 
class IsObject a => HasData a where
  -- | Get type of object
  getTypeIO      :: a -> HIO Type
  -- | Get dataspace associated with object
  getDataspaceIO :: a -> HIO Dataspace
  -- | Read all content of object
  unsafeReadAll  :: a      -- ^ Object handle
                 -> Type   -- ^ Type of in-memory elements 
                 -> Ptr () -- ^ Buffer to read to
                 -> HIO ()
  -- | Write full dataset at once
  unsafeWriteAll :: a      -- ^ Object handle
                 -> Type   -- ^ Type of in-memory elements
                 -> Ptr () -- ^ Buffer with data
                 -> HIO ()
                 
 
withDataspace :: (HasData a) => a -> (Dataspace -> HIO b) -> HIO b
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
    checkHID "Cannot read type from dataset" =<< h5d_get_type hid
  getDataspaceIO (Dataset hid) = Dataspace <$> do
    checkHID "Cannot read dataspace from dataset" =<< h5d_get_space hid
  unsafeReadAll (Dataset hid) ty buf = withType ty $ \tid ->
    checkHErr "Reading from dataset failed" =<< h5d_read hid tid
      h5s_ALL
      h5s_ALL
      h5p_DEFAULT
      (castPtr buf)
  unsafeWriteAll (Dataset hid) ty buf = withType ty $ \tid ->
    checkHErr "Reading to dataset failed" =<< h5d_write hid tid
      h5s_ALL
      h5s_ALL
      h5p_DEFAULT
      buf

instance HasAttrs Dataset

----------------

instance IsObject Attribute where
  getHID        = coerce
  unsafeFromHID = coerce
  getTag        = TagAttribute

instance HasData Attribute where
  getTypeIO (Attribute hid) = unsafeNewType $ do
    checkHID "Cannot read type from attribute" =<< h5a_get_type hid
  getDataspaceIO (Attribute hid) = Dataspace <$> do
    checkHID "Cannot read dataspace from dataset" =<< h5a_get_space hid
  unsafeReadAll (Attribute hid) ty buf = withType ty $ \tid ->
    checkHErr "Reading from attribute failed" =<<
      h5a_read hid tid (castPtr buf)
  unsafeWriteAll (Attribute hid) ty buf = withType ty $ \tid ->
    checkHErr "Writing of attribute failed" =<<
      h5a_write hid tid (castPtr buf)

----------------

instance IsObject Dataspace where
  getHID        = coerce
  unsafeFromHID = coerce
  getTag        = TagDataspace



instance Closable File where
  basicClose (File hid) = checkHErr "Unable to close file" =<< h5f_close hid
instance Closable Dataset where
  basicClose (Dataset hid) = checkHErr "Unable to close dataset" =<< h5d_close hid
instance Closable Attribute where
  basicClose (Attribute hid) = checkHErr "Unable to close attribute" =<< h5a_close hid
instance Closable Dataspace where
  basicClose (Dataspace hid) = checkHErr "Unable to close dataspace" =<< h5s_close hid


-- instance Closable Group where
--   closeIO (Group hid) = convertHErr "Unable to close group" $ h5g_close hid

