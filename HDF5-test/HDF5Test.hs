{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module HDF5Test where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Concurrent

import Data.Int
import Data.Char
import Data.Complex
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
import HDF5.HL.Internal.Dataspace
import HDF5.HL.Internal.Error
import HDF5.HL.Internal.Types
----------------------------------------------------------------


foo :: IO ()
foo = do
  withOpenFile "/run/user/1000/tst.hdf5" OpenRO $ \hdf -> do
    -- return ()
    withOpenDataset hdf "dset1" $ \dd -> do
      print =<< H5.getType dd
      print =<< H5.rank     dd
    --   -- print =<< H5.extent  dd
      print =<< H5.readDataset @[Int64] dd
      return ()
    -- putStrLn "\n----------------"
    -- withOpenDataset hdf "dset2" $ \dd -> do
    --   print =<< H5.getType  dd
    --   print =<< H5.rank dd
    --   -- print =<< H5.extent dd
    --   print =<< H5.readDataset @[Double] dd
    --   Just a1 <- H5.openAttr dd "a1"
    --   -- Just a2 <- H5.openAttr dd "a2"
    --   -- Just a3 <- H5.openAttr dd "a3"
    --   pure ()
    --   print =<< H5.getType     a1
    --   -- print =<< H5.extent      a1
    --   print =<< H5.readObject @Int32 a1
    -- putStrLn "\n----------------"
    withOpenDataset hdf "dset3" $ \dd -> do
      print =<< H5.getType dd
      mapM_ print =<< H5.readDataset @[Complex Double] dd


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


part :: IO ()
part = do
  H5.withCreateFile "/run/user/1000/tst.hdf5" H5.CreateTrunc $ \h5 -> do
    H5.createDataset h5 "d1" [0 .. 100::Int32]
    H5.withOpenDataset h5 "d1" $ \dset -> do
      print =<< H5.readSlab @[Int] dset 3 10
      writeSlab dset 10 (take 10 [1000::Int ..])
      print =<< readDataset @[Int] dset
      return ()
      -- print d
      -- evalContT $ do
      --   --
      --   p_err <- ContT alloca
      --   let off = 4  :: Int
      --       sz  = 12 :: Int
      --   -- Memory dataspace
      --   spc <- getDataspace d
      --   lift $ setSlabSelection spc off sz
      --   --
      --   mem <- ContT $ withCreateDataspace sz Nothing
      --   --
      --   tid   <- ContT $ withType (typeH5 @Int32)
      --   ptr   <- ContT $ allocaArray @Int32 100
      --   lift $ print =<< peekArray sz ptr
      --   lift $ checkHErr p_err "Reading dataset data failed"
      --        $ h5d_read (getHID d) tid
      --          (getHID mem)
      --          (getHID spc)
      --          H5P_DEFAULT (castPtr ptr)
      --   lift $ print =<< peekArray sz ptr
      --   --
      --   close spc
