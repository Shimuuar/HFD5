{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
-- |
module HDF5.HL
  ( -- * File API (H5F)
    File
  , open
  , withFile
    -- * Error handling
  , HDF5Error(..)
    -- * Type classes
  , Closable(..)
  , close
  ) where

import Control.Exception      (throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Foreign.C.String
import HDF5.HL.CCall
import HDF5.HL.Types
import HDF5.C                  qualified as C


-- | Open HDF5 file
open :: MonadIO m => FilePath -> OpenMode -> m File
open path mode = liftIO $ withCString path $ \c_path -> do
  hid <- C.h5f_open c_path (toCParam mode) C.h5p_DEFAULT
  when (hid == C.h5i_INVALID_HID)
    $ throwIO $ HDF5Error $ "Cannot open file " ++ path
  pure $ File hid

withFile
  :: (MonadMask m, MonadIO m)
  => FilePath -- ^ File path
  -> OpenMode
  -> (File -> m a)
  -> m a
withFile path mode = bracket (open path mode) close
