{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module HDF5Test where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Concurrent

import Data.Int
import Data.Char
import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Unboxed   qualified as FU
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array
import System.IO.Unsafe

import HDF5.C
import HDF5.HL as H5
import HDF5.HL.Internal.Wrappers
----------------------------------------------------------------


-- foo :: IO ()
-- foo = do
--   withOpenFile "/run/user/1000/tst.hdf5" OpenRO $ \hdf -> do
--     return ()
--     withDataset hdf "dset1" $ \dd -> do
--       print =<< H5.getType dd
--       print =<< H5.dim     dd
--       print =<< H5.extent  dd
--       print =<< H5.read @[Int64] dd
--       return ()
--     putStrLn "\n----------------"
--     withDataset hdf "dset2" $ \dd -> do
--       print =<< H5.getType  dd
--       print =<< H5.dim dd
--       print =<< H5.extent dd
--       print =<< H5.read @[Double] dd
--       Just a1 <- H5.openAttr dd "a1"
--       -- Just a2 <- H5.openAttr dd "a2"
--       -- Just a3 <- H5.openAttr dd "a3"
--       pure ()
--       print =<< H5.getType     a1
--       print =<< H5.extent      a1
--       print =<< H5.read @Int32 a1
--     putStrLn "\n----------------"
--     withDataset hdf "dset3" $ \dd -> do
--       print =<< H5.getType dd
--     --   --
--     --   print =<< H5.getType a2
--     --   print =<< H5.extent  a2
--     --   print =<< readScalar @Double a2
--     --   --
--     --   print =<< H5.getType a3
--     --   print =<< H5.dim     a3
--     --   print =<< H5.extent  a3
--     --   print =<< H5.read @[Double] a3
--     --   print =<< H5.read @[Int32] a3
--     --   -- print a3
--   --
--   pure ()


woo :: IO ()
woo = do
  H5.withCreateFile "/run/user/1000/tst.hdf5" H5.CreateTrunc $ \h5 -> do
    H5.createDataset h5 "d1" [1 .. 100::Int32]
    H5.createDataset h5 "d2" [1 .. 100::Int32]
    H5.createDataset h5 "d3" [1 .. 100::Int32]
    H5.withCreateGroup h5 "gg" $ \_ -> return ()
    pure ()
  H5.withOpenFile "/run/user/1000/tst.hdf5" H5.OpenRO $ \h5 -> do
    print h5
    putStrLn "----------------------------------------------------------------"
    alloca $ \p_idx -> alloca $ \p_err -> do
      poke p_idx 0
      callback <- makeH5LIterate2 $ \hid name p_node _ -> do
        print hid
        putStrLn =<< peekCString name
        peek (castPtr p_node) >>= putStrLn . \case
          H5L_TYPE_ERROR -> "H5L_TYPE_ERROR"
          H5L_TYPE_HARD -> "H5L_TYPE_HARD"
          H5L_TYPE_SOFT -> "H5L_TYPE_SOFT"
          H5L_TYPE_EXTERNAL -> "H5L_TYPE_EXTERNAL"
          H5L_TYPE_MAX -> "H5L_TYPE_MAX"
        return (HErr 0)
      print =<< h5l_iterate (getHID h5) H5_INDEX_NAME H5_ITER_INC p_idx callback nullPtr p_err
    print =<< H5.listGroup h5

    return ()
----------------------------------------------------------------


