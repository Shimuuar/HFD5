
{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
module HDF5.C.TypesC where

import Foreign.C

#include <hdf5.h>


----------------------------------------------------------------
-- Classes
----------------------------------------------------------------

newtype H5TClass = H5TClass CInt
  deriving (Show,Eq,Ord)


#{enum H5TClass, H5TClass,
  h5t_NO_CLASS  = H5T_NO_CLASS,
  h5t_INTEGER   = H5T_INTEGER,
  h5t_FLOAT     = H5T_FLOAT,
  h5t_TIME      = H5T_TIME,
  h5t_STRING    = H5T_STRING,
  h5t_BITFIELD  = H5T_BITFIELD,
  h5t_OPAQUE    = H5T_OPAQUE,
  h5t_COMPOUND  = H5T_COMPOUND,
  h5t_REFERENCE = H5T_REFERENCE,
  h5t_ENUM      = H5T_ENUM,
  h5t_VLEN      = H5T_VLEN,
  h5t_ARRAY     = H5T_ARRAY,
  h5t_NCLASSES  = H5T_NCLASSES
  }


----------------------------------------------------------------
-- Byte order
----------------------------------------------------------------

-- | Byte order for data type
newtype H5TOrder = H5TOrder CInt
  deriving (Show,Eq,Ord)

#{enum H5TOrder, H5TOrder,
  h5t_ORDER_ERROR = H5T_ORDER_ERROR,
  h5t_ORDER_LE    = H5T_ORDER_LE,
  h5t_ORDER_BE    = H5T_ORDER_BE,
  h5t_ORDER_VAX   = H5T_ORDER_VAX,
  h5t_ORDER_MIXED = H5T_ORDER_MIXED,
  h5t_ORDER_NONE  = H5T_ORDER_NONE
  }

-- | Error
pattern H5T_ORDER_ERROR :: H5TOrder
pattern H5T_ORDER_ERROR <- ((==h5t_ORDER_ERROR) -> True) where H5T_ORDER_ERROR = h5t_ORDER_ERROR

-- | Little endian
pattern H5T_ORDER_LE :: H5TOrder
pattern H5T_ORDER_LE <- ((==h5t_ORDER_LE) -> True) where H5T_ORDER_LE = h5t_ORDER_LE

-- | Big endian
pattern H5T_ORDER_BE :: H5TOrder
pattern H5T_ORDER_BE <- ((==h5t_ORDER_BE) -> True) where H5T_ORDER_BE = h5t_ORDER_BE

-- | VAX mixed endian
pattern H5T_ORDER_VAX :: H5TOrder
pattern H5T_ORDER_VAX <- ((==h5t_ORDER_VAX) -> True) where H5T_ORDER_VAX = h5t_ORDER_VAX

-- | Compound type with mixed member orders
pattern H5T_ORDER_MIXED :: H5TOrder
pattern H5T_ORDER_MIXED <- ((==h5t_ORDER_MIXED) -> True) where H5T_ORDER_MIXED = h5t_ORDER_MIXED

-- | no particular order (strings, bits,..)
pattern H5T_ORDER_NONE :: H5TOrder
pattern H5T_ORDER_NONE <- ((==h5t_ORDER_NONE) -> True) where H5T_ORDER_NONE = h5t_ORDER_NONE


----------------------------------------------------------------
-- Sign data
----------------------------------------------------------------

-- | Byte order for data type
newtype H5TSign = H5TSign CInt
  deriving (Show,Eq,Ord)


foreign import capi "hdf5.h value H5T_SGN_ERROR" h5t_SGN_ERROR :: H5TSign
foreign import capi "hdf5.h value H5T_SGN_NONE"  h5t_SGN_NONE  :: H5TSign
foreign import capi "hdf5.h value H5T_SGN_2"     h5t_SGN_2     :: H5TSign
foreign import capi "hdf5.h value H5T_NSGN"      h5t_NSGN      :: H5TSign

-- | Error
pattern H5T_SGN_ERROR :: H5TSign
pattern H5T_SGN_ERROR <- ((==h5t_SGN_ERROR) -> True) where H5T_SGN_ERROR = h5t_SGN_ERROR

-- | this is an unsigned type
pattern H5T_SGN_NONE :: H5TSign
pattern H5T_SGN_NONE <- ((==h5t_SGN_NONE) -> True) where H5T_SGN_NONE = h5t_SGN_NONE

-- | Two's complement
pattern H5T_SGN_2 :: H5TSign
pattern H5T_SGN_2 <- ((==h5t_SGN_2) -> True) where H5T_SGN_2 = h5t_SGN_2

pattern H5T_NSGN :: H5TSign
pattern H5T_NSGN <- ((==h5t_NSGN) -> True) where H5T_NSGN = h5t_NSGN
