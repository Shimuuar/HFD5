-- |
module Main (main) where

import Test.Tasty
import qualified TM.File

main :: IO ()
main = defaultMain $ testGroup "HDF5"
  [ TM.File.tests
  ]
