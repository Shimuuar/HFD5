{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- API for property lists
module HDF5.C.H5P where

import Foreign.C
import Foreign.Ptr
import HDF5.C.Types

foreign import capi "hdf5.h value H5P_DEFAULT" h5p_DEFAULT :: HID
