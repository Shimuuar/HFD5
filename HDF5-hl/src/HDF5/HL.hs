{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
module HDF5.HL
  ( -- * File API (H5F)
    File
  , open
  , withFile
    -- * Dataset API
  , dataset
  , withDataset
  , datasetType
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
-- import Control.Monad.Trans.Cont
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


-- | Open dataset
dataset
  :: (MonadIO m, Directory dir)
  => dir      -- ^ Root
  -> FilePath -- ^ Path relative to root
  -> m Dataset
dataset (directoryHID -> hid) path = liftIO $ do
  withCString path $ \c_path -> do
    r <- C.h5d_open2 hid c_path C.h5p_DEFAULT
    when (r == C.h5i_INVALID_HID)
      $ throwIO $ HDF5Error $ "Cannot open dataset " ++ path
    pure $ Dataset r

withDataset
  :: (MonadMask m, MonadIO m, Directory dir)
  => dir      -- ^ Root
  -> FilePath -- ^ Path relative to root
  -> (Dataset -> m a)
  -> m a
withDataset dir path = bracket (dataset dir path) close

datasetType
  :: (MonadIO m)
  => Dataset
  -> m Type
datasetType (Dataset hid) = liftIO $ do
  bracket (C.h5d_get_type hid) C.h5t_close $ \ty -> do
    (fromCEnum <$> C.h5t_get_class ty) >>= \case
      Just Integer -> do
        -- FIXME: error handling
        Just sign <- fromCEnum <$> C.h5t_get_sign ty
        sz   <- C.h5t_get_precision ty
        pure $ Integral sign (fromIntegral sz)
      Just c       -> error $ "Cannot handle type " ++ show c
      Nothing      -> error "Undecodable type"
  
