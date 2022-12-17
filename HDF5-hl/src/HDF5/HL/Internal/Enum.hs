{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Mapping of C enums from HDF to haskell data types
module HDF5.HL.Internal.Enum
  ( -- * Enumerations
    OpenMode(..)
  , Sign(..)
  , Class(..)
  ) where

import Foreign.C.Types
import HDF5.C qualified as C
import HDF5.HL.Internal.CCall


-- | Mode for opening files
data OpenMode
  = OpenRO -- ^ Open file in read-only mode
  | OpenRW -- ^ Open file in read-write mode
  deriving stock (Show, Eq)

instance HDF5Param OpenMode where
  type CParam OpenMode = CUInt
  toCParam OpenRO = C.h5f_ACC_RDONLY
  toCParam OpenRW = C.h5f_ACC_RDWR


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
