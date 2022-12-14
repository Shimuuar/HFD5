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
import Foreign.C.Types
import HDF5.C qualified as C
import HDF5.HL.CCall


----------------------------------------------------------------
-- Classes
----------------------------------------------------------------

-- | Most value (files, groups, datasets, etc.) should be closed
--   explicitly in order to avoid resource leaks.
class Closable a where
  closeIO :: a -> IO ()

close :: (Closable a, MonadIO m) => a -> m ()
close = liftIO . closeIO

class Directory a where
  directoryHID :: a -> C.HID


----------------------------------------------------------------
-- Files, groups, datasets
----------------------------------------------------------------

-- | Handle for working with HDF5 file
newtype File = File C.HID
  deriving stock (Show,Eq,Ord)

instance Closable File where
  closeIO (File hid) = convertHErr "Unable to close file" $ C.h5f_close hid
instance Directory File where
  directoryHID = coerce
-- newtype Group = Group C.HID
--   deriving stock (Show,Eq,Ord)

-- instance Closable Group where
--   closeIO (Group hid) = convertHErr "Unable to close group" $ C.h5g_close hid

data OpenMode
  = OpenRO
  | OpenRW
  deriving stock (Show, Eq)

instance HDF5Param OpenMode where
  type CParam OpenMode = CUInt
  toCParam OpenRO = C.h5f_ACC_RDONLY
  toCParam OpenRW = C.h5f_ACC_RDWR


newtype Dataset = Dataset C.HID
  deriving stock (Show,Eq,Ord)

instance Closable Dataset where
  closeIO (Dataset hid) = convertHErr "Unable to close dataset" $ C.h5d_close hid


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Representation of HDF5's data type.
--
--   Here we 
data Type
  = Integral !Sign !Word
  deriving stock (Show,Eq,Ord)

-- | Whether integral value is signed or not
data Sign
  = Signed
  | Unsigned
  deriving stock (Show,Eq,Ord)

instance HDF5Enum Sign where
  type CEnum Sign = C.H5TSign
  toCEnum Signed   = C.H5T_SGN_2
  toCEnum Unsigned = C.H5T_SGN_NONE
  fromCEnum = \case
    C.H5T_SGN_2    -> Just Signed
    C.H5T_SGN_NONE -> Just Unsigned
    _              -> Nothing

-- | Class of type
data Class
  = NoClass
  | Integer
  | Float
  | Time
  | String
  | BitField
  | Opaque
  | Compound
  | Reference
  | Enum
  | Vlen
  | Array
  deriving stock (Show,Eq,Ord)

instance HDF5Enum Class where
  type CEnum Class = C.H5TClass
  toCEnum = \case
    NoClass   -> C.H5T_NO_CLASS
    Integer   -> C.H5T_INTEGER
    Float     -> C.H5T_FLOAT
    Time      -> C.H5T_TIME
    String    -> C.H5T_STRING
    BitField  -> C.H5T_BITFIELD
    Opaque    -> C.H5T_OPAQUE
    Compound  -> C.H5T_COMPOUND
    Reference -> C.H5T_REFERENCE
    Enum      -> C.H5T_ENUM
    Vlen      -> C.H5T_VLEN
    Array     -> C.H5T_ARRAY
  fromCEnum = \case
    C.H5T_NO_CLASS  -> Just NoClass
    C.H5T_INTEGER   -> Just Integer    
    C.H5T_FLOAT     -> Just Float      
    C.H5T_TIME      -> Just Time       
    C.H5T_STRING    -> Just String     
    C.H5T_BITFIELD  -> Just BitField   
    C.H5T_OPAQUE    -> Just Opaque     
    C.H5T_COMPOUND  -> Just Compound   
    C.H5T_REFERENCE -> Just Reference  
    C.H5T_ENUM      -> Just Enum       
    C.H5T_VLEN      -> Just Vlen       
    C.H5T_ARRAY     -> Just Array      
    _               -> Nothing


----------------------------------------------------------------
-- Type casts and relations
----------------------------------------------------------------

data TypeHID
  = TyBuiltin !C.HID
  | TyIO      !(IO C.HID)
  
makeType :: Type -> TypeHID
makeType = \case
  Integral Signed   8  -> TyBuiltin C.h5t_NATIVE_SCHAR
  Integral Signed   16 -> TyBuiltin C.h5t_NATIVE_SHORT
  Integral Signed   32 -> TyBuiltin C.h5t_NATIVE_INT
  Integral Signed   64 -> TyBuiltin C.h5t_NATIVE_LONG
  Integral Unsigned 8  -> TyBuiltin C.h5t_NATIVE_UCHAR
  Integral Unsigned 16 -> TyBuiltin C.h5t_NATIVE_USHORT
  Integral Unsigned 32 -> TyBuiltin C.h5t_NATIVE_UINT
  Integral Unsigned 64 -> TyBuiltin C.h5t_NATIVE_ULONG
  -- FIXME:
  Integral _ _ -> error "Weird width integral types are not supported"
