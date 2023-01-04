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
  , unsafeRunHIO
    -- ** lifted functions
    -- *** Marshalling
  , hioPeek
  , hioPoke
  , hioPokeElemOff
  , hioPeekElemOff
  , hioPeekCString
  , hioWithCString
  , hioPeekArray
  , hioAlloca
  , hioAllocaArray
  , hioAllocaArray0
  , hioWithArray
    -- *** Foreign pointers
  , hioMallocForeignPtrArray
  , hioWithForeignPtr
    -- *** Function pointers
  , hioWithHaskellFunPtr
    -- *** IORef
  , hioNewIORef
  , hioReadIORef
  , hioModifyIORef'
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Data.Coerce
import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C
import Foreign.Marshal
import System.IO.Unsafe

import HDF5.C.Types
import HDF5.C.H5A
import HDF5.C.H5T
import HDF5.C.H5D
import HDF5.C.H5E
import HDF5.C.H5G
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

-- | Run HIO monad. Calls to @unsafeRunHIO@ must not be nested or it will
--   deadlock.
unsafeRunHIO :: MonadIO m => HIO a -> m a
unsafeRunHIO (HIO io)
  | is_threadsafe == 0 = liftIO $ withMVar mutex $ \_ -> do
      disabledAutoPrint `seq` io
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
  | otherwise          = liftIO io

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



----------------------------------------------------------------

hioPeek :: forall a. Storable a => Ptr a -> HIO a
hioPeek = coerce (peek @a)

hioPoke :: forall a. Storable a => Ptr a -> a -> HIO ()
hioPoke = coerce (poke @a)

hioPeekElemOff :: forall a. Storable a => Ptr a -> Int -> HIO a
hioPeekElemOff = coerce (peekElemOff @a)

hioPokeElemOff :: forall a. Storable a => Ptr a -> Int -> a -> HIO ()
hioPokeElemOff = coerce (pokeElemOff @a)

hioPeekCString :: CString -> HIO String
hioPeekCString = coerce peekCString

hioWithCString :: forall a. String -> (CString -> HIO a) -> HIO a
hioWithCString = coerce (withCString @a)

hioPeekArray :: forall a. Storable a => Int -> Ptr a -> HIO [a]
hioPeekArray = coerce (peekArray @a)

hioAlloca :: forall a r. Storable a => (Ptr a -> HIO r) -> HIO r
hioAlloca = coerce (alloca @a @r)

hioAllocaArray :: forall a r. Storable a => Int -> (Ptr a -> HIO r) -> HIO r
hioAllocaArray = coerce (allocaArray @a @r)

hioAllocaArray0 :: forall a r. Storable a => Int -> (Ptr a -> HIO r) -> HIO r
hioAllocaArray0 = coerce (allocaArray0 @a @r)

hioWithArray :: forall a r. Storable a => [a] -> (Ptr a -> HIO r) -> HIO r
hioWithArray = coerce (withArray @a @r)

hioNewIORef :: forall a. a -> HIO (IORef a)
hioNewIORef = coerce (newIORef @a)

hioMallocForeignPtrArray :: forall a. Storable a => Int -> HIO (ForeignPtr a)
hioMallocForeignPtrArray = coerce (mallocForeignPtrArray @a)

hioWithForeignPtr :: forall a r. ForeignPtr a -> (Ptr a -> HIO r) -> HIO r
hioWithForeignPtr = coerce (withForeignPtr @a @r)

hioWithHaskellFunPtr :: forall a r. HIO (FunPtr a) -> ContT r HIO (FunPtr a)
hioWithHaskellFunPtr mk = ContT $ \io -> HIO $ bracket (unHIO mk) freeHaskellFunPtr (unHIO . io)

hioReadIORef :: forall a. IORef a -> HIO a
hioReadIORef = coerce (readIORef @a)

hioModifyIORef' :: forall a. IORef a -> (a -> a) -> HIO ()
hioModifyIORef' = coerce (modifyIORef' @a)
