{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Utilities for calling C functions
module HDF5.HL.Internal.CCall
  ( -- * Exceptions
    HDF5Error(..)
  , ObjTag(..)
    -- ** Calling C functions
  , convertHErr
    -- * Internal type classes
  , HDF5Param(..)
  , HDF5Enum(..)
  ) where

import Control.Exception
import HDF5.C            qualified as C


----------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------

-- | Runtime for
data ObjTag
  = TagFile
  | TagGroup
  | TagDataset
  | TagAttribute
  | TagDataspace
  deriving (Show,Eq,Ord)

-- | Error during HDF5 call
data HDF5Error
  = HDF5Error String
  | CastError !ObjTag !ObjTag
  deriving stock Show

instance Exception HDF5Error

-- | Convert C error code to haskell exception
convertHErr
  :: String    -- ^ Error message in case of failure
  -> IO C.HErr -- ^ Function to call
  -> IO ()
-- FIXME: We need to extract full error stack from exception
convertHErr msg io = io >>= \case
  C.HErrored -> throwIO $ HDF5Error msg
  C.HOK      -> pure ()


-- | Conversion to and from corresponding C enumeration
class HDF5Enum a where
  type CEnum a
  fromCEnum :: CEnum a -> Maybe a
  toCEnum   :: a -> CEnum a

-- | Type class for values which could be converted to C
--   parameters. This type class is for value which only used as parametersq
class HDF5Param a where
  type CParam a
  toCParam :: a -> CParam a
