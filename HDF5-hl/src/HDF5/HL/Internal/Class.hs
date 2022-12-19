{-# LANGUAGE ImportQualifiedPost #-}
-- |
-- Type classes API
module HDF5.HL.Internal.Class
  ( IsObject(..)
  , IsDirectory(..)
  , Closable(..)
  , close
  ) where

import Control.Monad.IO.Class
import HDF5.C qualified as C

----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Most value (files, groups, datasets, etc.) should be closed
--   explicitly in order to avoid resource leaks. This is utility
--   class which allows to use same function to all of them.
class Closable a where
  closeIO :: a -> IO ()

-- | Lifted variant of 'closeIO'
close :: (Closable a, MonadIO m) => a -> m ()
close = liftIO . closeIO



-- | HDF5 object which has HID.
class IsObject a where
  getHID :: a -> C.HID

-- | HDF5 entities that could be used in context where group is
--   expected: groups, files (root group is used).
class IsObject a => IsDirectory a where
