{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Data types for working with HDF5 files
module HDF5.HL.Types
  ( -- * Files and groups
    File
  , OpenMode(..)
  , Dataset
  , Attribute
    -- * Data types
  , Type
    -- * Type classes
    -- ** HDF objects
  , IsObject
  , IsDirectory
  , HasData(..)
  , HasAttrs
    -- ** Closing objects
  , Closable(..)
  ) where

import HDF5.HL.Internal.Types
import HDF5.HL.Internal.TyHDF
