{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
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
    -- * IO wrapper
  , HIO(..)
  ) where


import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Int
import Foreign.C
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.TypeLits


----------------------------------------------------------------
-- Newtype wrappers
----------------------------------------------------------------

-- | Type of IDs to return to users
newtype HID = HID Int64
  deriving stock   (Show,Eq,Ord,Generic)
  deriving newtype (Storable)

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

-- | Newtype wrapper for IO for calling HDF5 functions. If HDF5 is
--   compiled in thread unsafe manner it rely on global variables and
--   calls must be protected by a mutex. @HIO@ allows to batch such
--   calls and to avoid races when one call fails and another clobbers
--   error stack before we're able to read its content.
--
--   Support for thread-safe HDF5 is not implemented yet.
--
newtype HIO a = HIO { unHIO :: IO a }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadThrow, MonadCatch, MonadMask)

-- | Instance is intentionally removed. This is to prevent nested
--   calls to 'HDF5.C.runHIO' which could lead to deadlocks. Otherwise
--   functions operating in 'MonadIO' could be used in 'HIO' context.
instance TypeError ('Text "HIO doesn't hame MonadIO istance on purpose") => MonadIO HIO where
  liftIO = error "UNREACHABLE"
