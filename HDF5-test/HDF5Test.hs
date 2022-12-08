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


----------------------------------------------------------------

type HID = C.HID

----------------------------------------------------------------

data HDF5Error = HDF5Error String
  deriving stock Show

instance Exception HDF5Error

data OpenMode
  = OpenRO
  | OpenRW
  deriving stock (Show, Eq)

open :: FilePath -> OpenMode -> IO HID
open path mode = withCString path $ \c_path -> do
  hid <- C.h5f_open c_path flag C.h5p_DEFAULT
  when (hid == C.h5i_INVALID_HID)
    $ throwIO $ HDF5Error $ "Cannot open file " ++ path
  pure hid
  where
    flag = case mode of OpenRO -> C.h5f_ACC_RDONLY
                        OpenRW -> C.h5f_ACC_RDWR

close :: HID -> IO ()
close hid = C.h5f_close hid >>= \case
  C.HErrored -> throwIO $ HDF5Error "Cannot close file"
  _          -> pure ()


withOpen :: FilePath -> OpenMode -> (HID -> IO a) -> IO a
withOpen path mode = bracket (open path mode) close




openDataset :: HID -> String -> IO HID
openDataset hid path = do
  withCString path $ \c_path -> do
    r <- C.h5d_open2 hid c_path C.h5p_DEFAULT
    when (r == C.h5i_INVALID_HID)
      $ throwIO $ HDF5Error $ "Cannot open dataset " ++ path
    pure r
    
closeDataset :: HID -> IO ()
closeDataset hid = C.h5d_close hid >>= \case
  C.HErrored -> throwIO $ HDF5Error "Cannot close dataset"
  _          -> pure ()

withDataset :: HID -> String -> (HID -> IO a) -> IO a
withDataset hid path = bracket (openDataset hid path) closeDataset


----------------------------------------------------------------

foo :: IO ()
foo = do
  withOpen "/run/user/1000/tst.hdf5" OpenRO $ \hid -> do
    print hid
    withDataset hid "dset1" $ \dset -> do
      print dset
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
      -- --
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
