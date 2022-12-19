{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module HDF5Test where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Cont

import Data.Int
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array

import HDF5.C qualified as C
import HDF5.HL as H5
import HDF5.HL.Types
import HDF5.HL.Internal.CCall
import HDF5.HL.Internal.TyHDF
import HDF5.HL.Internal.Types
----------------------------------------------------------------

type HID = C.HID



----------------------------------------------------------------

foo :: IO ()
foo = do
  withFile "/run/user/1000/tst.hdf5" OpenRO $ \hdf -> do
    withDataset hdf "dset1" $ \dd@(Dataset dset) -> do
      print =<< H5.getType  dd
      print =<< H5.dim dd
      print =<< H5.extent dd
      print =<< H5.read @[Int64] dd
    putStrLn "\n----------------"
    withDataset hdf "dset2" $ \dd@(Dataset dset) -> do
      print =<< H5.getType  dd
      print =<< H5.dim dd
      print =<< H5.extent dd
      print =<< H5.read @[Double] dd
      -- print a3
  --
  pure ()

----------------------------------------------------------------

-- tyty :: IO ()
-- tyty = do
--   arr <- alloca @C.HSize $ \dims -> do
--     poke dims 12
--     C.h5t_array_create C.h5t_NATIVE_FLOAT 1 dims
--   arr2 <- alloca @C.HSize $ \dims -> do
--     poke dims 133
--     C.h5t_array_create arr 1 dims
--   print =<< C.h5t_close arr
--   --
--   print =<< (fromCEnum @Class <$> C.h5t_get_class arr2)
--   ty <- C.h5t_get_super arr2
--   print =<< (fromCEnum @Class <$> C.h5t_get_class ty)
--   tyty <- C.h5t_get_super ty
--   print =<< (fromCEnum @Class <$> C.h5t_get_class tyty)
--   --
--   print =<< C.h5t_close tyty
--   print =<< C.h5t_close ty
--   print =<< C.h5t_close arr2
--   return ()

----------------------------------------------------------------
