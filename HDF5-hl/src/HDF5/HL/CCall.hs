{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
module HDF5.HL.CCall where

import Control.Exception
import HDF5.C            qualified as C

-- | Error during HDF5 call
data HDF5Error = HDF5Error String
  deriving stock Show

instance Exception HDF5Error

convertHErr :: String -> IO C.HErr -> IO ()
convertHErr msg io = io >>= \case
  C.HErrored -> throwIO $ HDF5Error msg
  C.HOK      -> pure ()


-- | Conversion to and from corresponding C enumeration
class HDF5Enum a where
  type CEnum a
  fromCEnum :: CEnum a -> a
  toCEnum   :: a -> CEnum a

-- | Type class for values which could be converted to C
--   parameters. This type class is for value which only used as parametersq
class HDF5Param a where
  type CParam a
  toCParam :: a -> CParam a
