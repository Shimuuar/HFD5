{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
module HDF5.C
  ( -- * Data types
    module HDF5.C.Types
    -- * Properties API
    -- ** Constants
  , h5p_DEFAULT
  , h5i_INVALID_HID
    -- * File API
  , module HDF5.C.H5F
    -- * Dataset API
  , module HDF5.C.H5D
    -- * Datatypes API
  , module HDF5.C.H5T
    -- * Dataspace API
  , module HDF5.C.H5S
  ) where

import Foreign.C

import HDF5.C.Types
import HDF5.C.H5T
import HDF5.C.H5D
import HDF5.C.H5S
import HDF5.C.H5F


----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

foreign import capi "hdf5.h value H5P_DEFAULT"        h5p_DEFAULT     :: HID
foreign import capi "hdf5.h value H5I_INVALID_HID"    h5i_INVALID_HID :: HID
