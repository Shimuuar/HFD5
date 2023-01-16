{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE ViewPatterns             #-}
-- |
module HDF5.C
  ( -- * Data types
    module HDF5.C.Types
    -- * Properties API
    -- ** Constants
  , h5p_DEFAULT
  , h5i_INVALID_HID
  , h5s_ALL
    -- * Attributes API
  , module HDF5.C.H5A
    -- * Group API
  , module HDF5.C.H5G
    -- * Links API
  , module HDF5.C.H5L
    -- * File API
  , module HDF5.C.H5F
    -- * Dataset API
  , module HDF5.C.H5D
    -- * Datatypes API
  , module HDF5.C.H5T
    -- * Dataspace API
  , module HDF5.C.H5S
    -- * High level API
  , module HDF5.C.H5LT
    -- * Error handling API
  , module HDF5.C.H5E
    -- * IO wrapper
  , is_threadsafe
  ) where

import HDF5.C.Types
import HDF5.C.H5A
import HDF5.C.H5T
import HDF5.C.H5D
import HDF5.C.H5E
import HDF5.C.H5G
import HDF5.C.H5S
import HDF5.C.H5L
import HDF5.C.H5F
import HDF5.C.H5LT

----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

foreign import capi "hdf5.h value H5P_DEFAULT"        h5p_DEFAULT     :: HID
foreign import capi "hdf5.h value H5I_INVALID_HID"    h5i_INVALID_HID :: HID
foreign import capi "hdf5.h value H5S_ALL"            h5s_ALL         :: HID


foreign import capi "hdf5-hs.h value HS_H5_THREADSAFE" is_threadsafe :: Int

{-
disabledAutoPrint :: HErr
{-# NOINLINE disabledAutoPrint #-}
disabledAutoPrint
  = unsafePerformIO
  $ h5e_set_auto h5e_DEFAULT nullFunPtr nullPtr
-}
