{-# LANGUAGE LambdaCase #-}
-- |
module HDF5.HL.Internal.Property
  ( -- * Property lists
    Property(..)
    -- * Dataset properties
  , withDatasetProps
  , datasetLayout
  , datasetChunking
  ) where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Foreign.Ptr
import Foreign.Marshal
import GHC.Stack

import HDF5.C
import HDF5.HL.Internal.Enum
import HDF5.HL.Internal.Error
import HDF5.HL.Internal.Wrappers
import HDF5.HL.Internal.Dataspace


----------------------------------------------------------------
-- Property lists
----------------------------------------------------------------

-- | Property lists for values of type p
data Property p = NoProperty
                | Property (Ptr HID -> PropertyHID p -> IO ())

instance Semigroup (Property p) where
  NoProperty <> p          = p
  p          <> NoProperty = p
  Property f <> Property g = Property $ (liftA2 . liftA2) (>>) f g
  
instance Monoid (Property p) where
  mempty = NoProperty


----------------------------------------------------------------
-- Dataset
----------------------------------------------------------------

withDatasetProps :: Property Dataset -> (PropertyHID Dataset -> IO a) -> IO a
withDatasetProps prop action = case prop of
  NoProperty -> action $ PropertyHID H5P_DEFAULT
  Property f -> alloca $ \p_err -> do
    let open = fmap PropertyHID
             $ checkHID p_err "Unable to create property list"
             $ h5p_create H5P_DATASET_CREATE
    bracket open basicClose $ \p -> f p_err p >> action p

-- | Set up dataset layout
datasetLayout :: HasCallStack => Layout -> Property Dataset
datasetLayout l = Property $ \p_err p -> withFrozenCallStack
  $ checkHErr p_err "Unable to set layout for dataset"
  $ h5p_set_layout (getHID p) (toCEnum l)
  
-- | Set chunking for a dataset
datasetChunking :: (HasCallStack, IsExtent dim) => dim -> Property Dataset
datasetChunking dim = Property $ \p_err prop -> withFrozenCallStack $ evalContT $ do
  withEncodedExtent dim >>= \case
    Nothing       -> throwM $ Error [Left "Extent must be non-null"]
    Just (rank,p) -> lift
      $ checkHErr p_err "Unable to set chunk size"
      $ h5p_set_chunk (getHID prop) (fromIntegral rank) p
