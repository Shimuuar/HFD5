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
  , h5s_ALL
    -- * Attributes API
  , module HDF5.C.H5A
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
  , HIO(..)
  , runHIO
  , liftHIO
  , liftHIOBracket
  ) where

import Control.Concurrent.MVar
import Control.Monad.Trans.Cont
import Data.Coerce
import Foreign.Ptr
import System.IO.Unsafe

import HDF5.C.Types
import HDF5.C.H5A
import HDF5.C.H5T
import HDF5.C.H5D
import HDF5.C.H5E
import HDF5.C.H5S
import HDF5.C.H5F
import HDF5.C.H5LT

----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

foreign import capi "hdf5.h value H5P_DEFAULT"        h5p_DEFAULT     :: HID
foreign import capi "hdf5.h value H5I_INVALID_HID"    h5i_INVALID_HID :: HID
foreign import capi "hdf5.h value H5S_ALL"            h5s_ALL         :: HID


----------------------------------------------------------------
-- HIO & friends
----------------------------------------------------------------

-- | Run HIO monad. Calls to @runHIO@ must not be nested or it will
--   deadlock.
runHIO :: HIO a -> IO a
runHIO (HIO io)
  | is_threadsafe == 0 = withMVar mutex $ \_ -> disabledAutoPrint `seq` io
  -- FIXME: calls are safe but error handling is wrong. HDF5 error
  --        stack is stored in thread local storage and our thread may
  --        migrate to another capability and we'll look at wrong
  --        stack.
  --
  --        Proper support for threaded library will most likely
  --        require wrapping each HDF5 call into C function which will
  --        check for errors
  --
  -- FIXME: Also need to figure out how h5e_set_auto works.
  | otherwise          = io

-- | Lift bracket in IO monad into bracket working in HIO. Generally
--   useful for dealing with functions like @alloca@.
liftHIO :: ContT r IO a -> ContT r HIO a
liftHIO = coerce

-- | Lift bracket in IO monad into bracket working in HIO. Generally
--   useful for dealing with functions like @alloca@.
liftHIOBracket
  :: ((a ->  IO b) ->  IO b)
  -> ((a -> HIO b) -> HIO b)
liftHIOBracket = coerce

mutex :: MVar ()
{-# NOINLINE mutex #-}
mutex = unsafePerformIO $ newMVar ()

foreign import capi "hdf5-hs.h value HS_H5_THREADSAFE" is_threadsafe :: Int


disabledAutoPrint :: HErr
{-# NOINLINE disabledAutoPrint #-}
disabledAutoPrint
  = unsafePerformIO
  $ unHIO
  $ h5e_set_auto h5e_DEFAULT nullFunPtr nullPtr
