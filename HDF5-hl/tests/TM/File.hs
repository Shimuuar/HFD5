{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications    #-}
-- |
module TM.File (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp
import System.FilePath   ((</>))
import HDF5.HL           qualified as H5

tests :: TestTree
tests = testGroup "Files"
  [ testCase "Create/open file" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
      H5.withCreateFile path H5.CreateTrunc $ \_ -> pure ()
      H5.withOpenFile   path H5.OpenRO      $ \_ -> pure ()
    --
  , testCase "Write/read dataset" $ withDir $ \dir -> do
      let path   = dir </> "test.h5"
          arr    = [1 .. 10] :: [Int]
          scalar = 123 :: Double
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        H5.createDataset h5 "dset1" arr
        H5.createDataset h5 "dset2" scalar
      H5.withOpenFile   path H5.OpenRO      $ \h5 -> do
        assertEqual "Array"  arr    =<< H5.readAt h5 "dset1"
        assertEqual "Scalar" scalar =<< H5.readAt h5 "dset2"
    --
  , testCase "Write/read attribute (root)" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
          val  = 123.25 :: Double
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        H5.createAttr h5 "attr" val
      H5.withOpenFile   path H5.OpenRO      $ \h5 -> do
        assertEqual "attribute"   (Just val)        =<< H5.readAttr h5 "attr"
        assertEqual "attribute"   (Just (123::Int)) =<< H5.readAttr h5 "attr"
        assertEqual "Nonexistent" (Nothing @Int)    =<< H5.readAttr h5 "attr-X"
    --
  , testCase "Write/read slabs" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
          arr  = [0..10] :: [Int]
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        H5.withCreateDataset h5 "dset1" arr $ \dset -> do
          assertEqual "Slice" [3,4,5] =<< H5.readSlab @[Int] dset 3 3
          H5.writeSlab dset 3 [30,40,50::Int]
          assertEqual "After write" [0,1,2,30,40,50,6,7,8,9,10] =<< H5.readDataset @[Int] dset
    --
  , testCase "Resize/chunking" $ withDir $ \dir -> do
      let path = dir </> "test.h5"
      H5.withCreateFile path H5.CreateTrunc $ \h5 -> do
        H5.withCreateEmptyDataset h5 "dset" (H5.typeH5 @Int) 0 (Just H5.UNLIMITED)
          [H5.propDatasetChunking (10::Int)] $ \dset -> do
            -- 1-st resize
            H5.setDatasetExtent dset (10::Int)
            assertEqual "Zero" (replicate 10 0) =<< H5.readDataset @[Int] dset
            -- 1-st write
            H5.writeSlab dset 0 (take 10 [0::Int ..])
            assertEqual "Zero" [0..9] =<< H5.readDataset @[Int] dset
            -- 2-nd resize & write
            H5.setDatasetExtent dset (20::Int)
            H5.writeSlab dset 10 (take 10 [100::Int ..])
            assertEqual "Zero" ([0..9]++[100..109]) =<< H5.readDataset @[Int] dset
  ]

withDir :: (FilePath -> IO a) -> IO a
withDir = withSystemTempDirectory "HDF5"
