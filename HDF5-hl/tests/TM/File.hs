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
  ]

withDir :: (FilePath -> IO a) -> IO a
withDir = withSystemTempDirectory "HDF5"
