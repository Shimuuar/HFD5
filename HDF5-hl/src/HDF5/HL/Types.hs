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
  -- , Type(..)
  , Sign(..)
  , Class(..)
    -- * Exceptions
  , HDF5Error(..)
    -- * Type classes
  , Closable(..)
  , close
  , IsObject(..)
  , IsDirectory(..)
   --
  -- , TypeHID(..)
  -- , makeType
  ) where

import Control.Monad.IO.Class
import Data.Coerce
import HDF5.C qualified as C
import HDF5.HL.Internal.CCall
import HDF5.HL.Internal.Enum
import HDF5.HL.Internal.Class

----------------------------------------------------------------
-- Files, groups, datasets
----------------------------------------------------------------

-- | Handle for working with HDF5 file
newtype File = File C.HID
  deriving stock (Show,Eq,Ord)

-- | Handle for dataset
newtype Dataset = Dataset C.HID
  deriving stock (Show,Eq,Ord)


instance IsObject    File where getHID = coerce
instance IsDirectory File

instance IsObject Dataset where getHID = coerce

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



-- ----------------------------------------------------------------
-- -- Types
-- ----------------------------------------------------------------

-- -- | Representation of HDF5's data type.
-- --
-- --   Here we 
-- data Type
--   = Integral !Sign !Word
--   deriving stock (Show,Eq,Ord)




-- ----------------------------------------------------------------
-- -- Type casts and relations
-- ----------------------------------------------------------------

-- data TypeHID
--   = TyBuiltin !C.HID
--   | TyMade    !C.HID
  
-- makeType :: Type -> IO TypeHID
-- makeType = \case
--   Integral Signed   8  -> pure $ TyBuiltin C.h5t_NATIVE_SCHAR
--   Integral Signed   16 -> pure $ TyBuiltin C.h5t_NATIVE_SHORT
--   Integral Signed   32 -> pure $ TyBuiltin C.h5t_NATIVE_INT
--   Integral Signed   64 -> pure $ TyBuiltin C.h5t_NATIVE_LONG
--   Integral Unsigned 8  -> pure $ TyBuiltin C.h5t_NATIVE_UCHAR
--   Integral Unsigned 16 -> pure $ TyBuiltin C.h5t_NATIVE_USHORT
--   Integral Unsigned 32 -> pure $ TyBuiltin C.h5t_NATIVE_UINT
--   Integral Unsigned 64 -> pure $ TyBuiltin C.h5t_NATIVE_ULONG
--   -- FIXME:
--   Integral _ _ -> error "Weird width integral types are not supported"
