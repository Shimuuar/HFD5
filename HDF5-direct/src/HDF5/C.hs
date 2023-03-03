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
  , h5i_INVALID_HID
    -- * General library
  , h5_free_memory
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
    -- * Property lists
  , module HDF5.C.H5P
    -- * IO wrapper
  , is_threadsafe
  ) where

import Foreign.Ptr
import Foreign.C

import HDF5.C.Types
import HDF5.C.H5A
import HDF5.C.H5T
import HDF5.C.H5D
import HDF5.C.H5E
import HDF5.C.H5G
import HDF5.C.H5S
import HDF5.C.H5L
import HDF5.C.H5F
import HDF5.C.H5P
import HDF5.C.H5LT

----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

foreign import capi "hdf5.h value H5I_INVALID_HID"    h5i_INVALID_HID :: HID

foreign import capi "hdf5-hs.h value HS_H5_THREADSAFE" is_threadsafe :: Int

-- | @H5free_memory@ frees memory that has been allocated by the
--   caller with @H5allocate_memory@ or by the HDF5 library on behalf
--   of the caller.
--
--   @H5Tget_member_name@ provides an example of memory allocation on
--   behalf of the caller: The function returns a buffer containing
--   the name of a compound datatype member. It is the caller’s
--   responsibility to eventually free that buffer with
--   @H5free_memory@.
foreign import capi "hdf5.h H5free_memory" h5_free_memory
  :: Ptr x
  -> IO HErr
