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
    return ()
    withDataset hdf "dset1" $ \dd -> do
      print =<< H5.getType dd
      print =<< H5.dim     dd
      print =<< H5.extent  dd
      print =<< H5.read @[Int64] dd
      return ()
    putStrLn "\n----------------"
    withDataset hdf "dset2" $ \dd -> do
      print =<< H5.getType  dd
      print =<< H5.dim dd
      print =<< H5.extent dd
      print =<< H5.read @[Double] dd
      Just a1 <- H5.openAttr dd "a1"
      -- Just a2 <- H5.openAttr dd "a2"
      -- Just a3 <- H5.openAttr dd "a3"
      pure ()
      print =<< H5.getType     a1
      print =<< H5.extent      a1
      print =<< H5.read @Int32 a1
    putStrLn "\n----------------"
    withDataset hdf "dset3" $ \dd -> do
      print =<< H5.getType dd
    --   --
    --   print =<< H5.getType a2
    --   print =<< H5.extent  a2
    --   print =<< readScalar @Double a2
    --   --
    --   print =<< H5.getType a3
    --   print =<< H5.dim     a3
    --   print =<< H5.extent  a3
    --   print =<< H5.read @[Double] a3
    --   print =<< H5.read @[Int32] a3
    --   -- print a3
  --
  pure ()


woo :: IO ()
woo = do
  withFile "/run/user/1000/tst.hdf5" OpenRW $ \hdf -> do
    H5.writeAt hdf "ddd" [1 .. 100::Int32]
    H5.withDataset hdf "ddd" $ \dset -> do
      createAttr dset "a1" (1 :: Float)
      createAttr dset "a2/aa" (1.222 :: Float)

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
