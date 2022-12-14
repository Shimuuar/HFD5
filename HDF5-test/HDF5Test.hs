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
import HDF5.HL
import HDF5.HL.Types

----------------------------------------------------------------

type HID = C.HID



----------------------------------------------------------------

foo :: IO ()
foo = do
  withFile "/run/user/1000/tst.hdf5" OpenRO $ \hdf -> do
    withDataset hdf "dset1" $ \dd@(Dataset dset) -> do
      print dset
      print =<< datasetType dd
      ty  <- C.h5d_get_type dset
      spc <- C.h5d_get_space dset
      sz  <- C.h5t_get_size ty
      --
      putStrLn "---- TYPE ----"
      print ty
      print sz
      print =<< C.h5t_get_class ty
      print =<< C.h5t_get_order ty
      print =<< C.h5t_get_precision ty
      print =<< C.h5t_get_sign ty
      putStrLn "---- SPACE ----"
      print spc
      print =<< C.h5s_get_simple_extent_ndims spc
      alloca $ \p1 -> alloca $ \p2 -> do
        print =<< C.h5s_get_simple_extent_dims spc p1 p2
        print =<< peek p1
        print =<< peek p2
      -- DATA
      allocaArray @Int32 100 $ \buf -> do
        C.h5d_read dset
          C.h5t_NATIVE_INT
          C.h5s_ALL
          C.h5s_ALL
          C.h5p_DEFAULT
          (castPtr buf)
        print =<< peekArray 100 buf
      -- 2
      allocaArray @Int64 100 $ \buf -> do
        C.h5d_read dset
          C.h5t_NATIVE_LONG
          C.h5s_ALL
          C.h5s_ALL
          C.h5p_DEFAULT
          (castPtr buf)
        print =<< peekArray 100 buf
      -- 3
      allocaArray @Int16 100 $ \buf -> do
        C.h5d_read dset
          C.h5t_NATIVE_SHORT
          C.h5s_ALL
          C.h5s_ALL
          C.h5p_DEFAULT
          (castPtr buf)
        print =<< peekArray 100 buf
      -- 3
      allocaArray @Int8 100 $ \buf -> do
        C.h5d_read dset
          C.h5t_NATIVE_SCHAR
          C.h5s_ALL
          C.h5s_ALL
          C.h5p_DEFAULT
          (castPtr buf)
        print =<< peekArray 100 buf
      --
      C.h5t_close ty
      C.h5s_close spc
      return ()
      -- print "---"
      -- print C.h5t_NATIVE_SCHAR
      -- print C.h5t_NATIVE_UCHAR
      -- print C.h5t_NATIVE_SHORT
      -- print C.h5t_NATIVE_USHORT
      -- print C.h5t_NATIVE_INT
      -- print C.h5t_NATIVE_UINT
      -- print C.h5t_NATIVE_LONG
      -- print C.h5t_NATIVE_ULONG
    

  return ()

----------------------------------------------------------------

cd :: String -> ContT r IO String
cd s = ContT $ bracket (s <$ putStrLn (">>> "++s)) (\_ -> putStrLn ("<<< "++s))

bar :: IO ()
bar = evalContT $ do
  cd "A"
  cd "B"
  resetT $ cd " C" >> cd " D"
  cd "E"
  return ()
