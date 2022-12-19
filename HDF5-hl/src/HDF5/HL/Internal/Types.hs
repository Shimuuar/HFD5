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
  , Dataset(..)
  , Attribute(..)
  , Dataspace(..)
    -- * Dimensions
  , Dim(..)
  , Extent(..)
    -- * Exceptions
  , HDF5Error(..)
    -- * Type classes
  , Closable(..)
  , close
  , IsObject(..)
  , castObj
  , castObj'
  , IsDirectory
  , HasData(..)
  , getType
  , getDataspace
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Coerce
import Data.Int
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
class IsObject a => IsDirectory a where

-- | HDF5 entities which contains data that could be 
class IsObject a => HasData a where
  getTypeIO      :: a -> IO Type
  getDataspaceIO :: a -> IO Dataspace

getType :: (HasData a, MonadIO m) => a -> m Type
getType = liftIO . getTypeIO

getDataspace :: (HasData a, MonadIO m) => a -> m Dataspace
getDataspace = liftIO . getDataspaceIO


----------------------------------------------------------------
-- Dimensions
----------------------------------------------------------------

data Dim = Dim
  { dimSize    :: !Int64
  , dimMaxSize :: !Int64
  }
  deriving stock (Show,Eq,Ord)

newtype Extent = Extent [Dim]
  deriving stock (Show,Eq,Ord)


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

instance IsObject Group where
  getHID        = coerce
  unsafeFromHID = coerce
  getTag        = TagGroup
instance IsDirectory Group

instance IsObject Dataset where
  getHID        = coerce
  unsafeFromHID = coerce
  getTag        = TagDataset
instance HasData Dataset where
  getTypeIO (Dataset hid) = unsafeNewType $ do
    checkINV "Cannot read type from dataset" =<< C.h5d_get_type hid
  getDataspaceIO (Dataset hid) = Dataspace <$> do
    checkINV "Cannot read dataspace from dataset" =<< C.h5d_get_space hid

instance IsObject Attribute where
  getHID        = coerce
  unsafeFromHID = coerce
  getTag        = TagAttribute
instance HasData Attribute where
  getTypeIO (Attribute hid) = unsafeNewType $ do
    checkINV "Cannot read type from attribute" =<< C.h5a_get_type hid
  getDataspaceIO (Attribute hid) = Dataspace <$> do
    checkINV "Cannot read dataspace from dataset" =<< C.h5a_get_space hid

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


checkINV :: String -> C.HID -> IO C.HID
checkINV msg hid
  | hid == C.h5i_INVALID_HID = throwIO $ HDF5Error msg
  | otherwise                = pure hid

-- instance Closable Group where
--   closeIO (Group hid) = convertHErr "Unable to close group" $ C.h5g_close hid

