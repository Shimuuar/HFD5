{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Data types for working with HDF5 files
module HDF5.HL.Types
  ( -- * Files and groups
    File(..)
  , OpenMode(..)
  , Dataset(..)
    -- * Data types
  , Type(..)
  , Sign(..)
  , Class(..)
    -- * Exceptions
  , HDF5Error(..)
    -- * Type classes
  , Closable(..)
  , close
  , Directory(..)
   --
  , TypeHID(..)
  , makeType
  ) where

import Control.Monad.IO.Class
import Data.Coerce
import HDF5.C qualified as C
import HDF5.HL.Internal.CCall
import HDF5.HL.Internal.Enum

----------------------------------------------------------------
-- Classes
----------------------------------------------------------------

-- | Most value (files, groups, datasets, etc.) should be closed
--   explicitly in order to avoid resource leaks. This is utility
--   class which allows to use same function to all of them.
class Closable a where
  closeIO :: a -> IO ()

-- | Lifted variant of 'closeIO'
close :: (Closable a, MonadIO m) => a -> m ()
close = liftIO . closeIO

-- | HDF5 entities that could be used in context where group is
--   expected: groups, files (root group is used).
class Directory a where
  directoryHID :: a -> C.HID


----------------------------------------------------------------
-- Files, groups, datasets
----------------------------------------------------------------

-- | Handle for working with HDF5 file
newtype File = File C.HID
  deriving stock (Show,Eq,Ord)

-- | Handle for dataset
newtype Dataset = Dataset C.HID
  deriving stock (Show,Eq,Ord)


instance Directory File where
  directoryHID = coerce


instance Closable File where
  closeIO (File hid) = convertHErr "Unable to close file" $ C.h5f_close hid
instance Closable Dataset where
  closeIO (Dataset hid) = convertHErr "Unable to close dataset" $ C.h5d_close hid


-- newtype Group = Group C.HID
--   deriving stock (Show,Eq,Ord)

-- instance Closable Group where
--   closeIO (Group hid) = convertHErr "Unable to close group" $ C.h5g_close hid

----------------------------------------------------------------
-- Enumerations
----------------------------------------------------------------



----------------------------------------------------------------
-- Types
----------------------------------------------------------------

-- | Representation of HDF5's data type.
--
--   Here we 
data Type
  = Integral !Sign !Word
  deriving stock (Show,Eq,Ord)




----------------------------------------------------------------
-- Type casts and relations
----------------------------------------------------------------

data TypeHID
  = TyBuiltin !C.HID
  | TyMade    !C.HID
  
makeType :: Type -> IO TypeHID
makeType = \case
  Integral Signed   8  -> pure $ TyBuiltin C.h5t_NATIVE_SCHAR
  Integral Signed   16 -> pure $ TyBuiltin C.h5t_NATIVE_SHORT
  Integral Signed   32 -> pure $ TyBuiltin C.h5t_NATIVE_INT
  Integral Signed   64 -> pure $ TyBuiltin C.h5t_NATIVE_LONG
  Integral Unsigned 8  -> pure $ TyBuiltin C.h5t_NATIVE_UCHAR
  Integral Unsigned 16 -> pure $ TyBuiltin C.h5t_NATIVE_USHORT
  Integral Unsigned 32 -> pure $ TyBuiltin C.h5t_NATIVE_UINT
  Integral Unsigned 64 -> pure $ TyBuiltin C.h5t_NATIVE_ULONG
  -- FIXME:
  Integral _ _ -> error "Weird width integral types are not supported"
