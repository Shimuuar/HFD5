{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
-- Newtype wrappers over data types defined by C API of HDF5.
module HDF5.C.Types
  ( -- * Data types and patters
    HID(..)
  , HTri(..)
  , pattern HTrue
  , pattern HFalse
  , pattern HFail
  , HErr(..)
  , pattern HOK
  , pattern HErrored
  , HSize
  , HSSize
    -- * Types
    -- ** Classes of types
  , H5TClass(..)
  , h5t_NO_CLASS
  , h5t_INTEGER
  , h5t_FLOAT
  , h5t_TIME
  , h5t_STRING
  , h5t_BITFIELD
  , h5t_OPAQUE
  , h5t_COMPOUND
  , h5t_REFERENCE
  , h5t_ENUM
  , h5t_VLEN
  , h5t_ARRAY
  , h5t_NCLASSES
    -- ** Byte order
  , H5TOrder(..)
  , pattern H5T_ORDER_ERROR
  , pattern H5T_ORDER_LE
  , pattern H5T_ORDER_BE
  , pattern H5T_ORDER_VAX
  , pattern H5T_ORDER_MIXED
  , pattern H5T_ORDER_NONE
    -- ** Sign information
  , H5TSign(..)
  , pattern H5T_SGN_ERROR
  , pattern H5T_SGN_NONE
  , pattern H5T_SGN_2
  , pattern H5T_NSGN
  ) where

import Data.Int
import Foreign.C
import GHC.Generics (Generic)
import HDF5.C.TypesC

----------------------------------------------------------------
-- Newtype wrappers
----------------------------------------------------------------

-- | Type of IDs to return to users
newtype HID = HID Int64
  deriving stock (Show,Eq,Ord,Generic)

-- | Three-valued Boolean type. Functions that return htri_t however
--   return zero (false), positive (true), or negative (failure).
newtype HTri = HTri CInt
  deriving stock (Show,Eq,Ord,Generic)

pattern HTrue, HFalse, HFail :: HTri
pattern HFalse = HTri 0
pattern HTrue  <- HTri ((>0) -> True)
pattern HFail  <- HTri ((<0) -> True)
{-# COMPLETE HTrue, HFalse, HFail #-}

-- | Status return values. Failed integer functions in HDF5 result
--   almost always in a negative value (unsigned failing functions
--   sometimes return zero for failure) while successful return is
--   non-negative (often zero). The negative failure value is most
--   commonly -1, but don't bet on it.
newtype HErr = HErr CInt
  deriving stock (Show,Eq,Ord,Generic)

pattern HErrored, HOK :: HErr
pattern HErrored <- HErr ((<0)  -> True)
pattern HOK      <- HErr ((>=0) -> True)
{-# COMPLETE HErrored, HOK #-}


type HSize  = CULong
type HSSize = CLong
