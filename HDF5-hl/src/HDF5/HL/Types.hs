{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Data types for working with HDF5 files
module HDF5.HL.Types
  ( -- * Files and groups
    File(..)
  , OpenMode(..)
    -- * Exceptions
  , HDF5Error(..)
    -- * Type classes
  , Closable(..)
  , close
  ) where

import Control.Monad.IO.Class
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


----------------------------------------------------------------
-- Files, groups, datasets
----------------------------------------------------------------

-- | Handle for working with HDF5 file
newtype File = File C.HID
  deriving stock (Show,Eq,Ord)

instance Closable File where
  closeIO (File hid) = convertHErr "Unable to close file" $ C.h5f_close hid

data OpenMode
  = OpenRO
  | OpenRW
  deriving stock (Show, Eq)

instance HDF5Param OpenMode where
  type CParam OpenMode = CUInt
  toCParam OpenRO = C.h5f_ACC_RDONLY
  toCParam OpenRW = C.h5f_ACC_RDWR
