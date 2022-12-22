{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Description of dataspaces.
module HDF5.HL.Internal.Dataspace
  ( -- * Concrete type
    Dim(..)
  , Extent(..)
    -- * Helpers for working with datasets
  , DSpaceWriter
  , putDimension
  , withDSpace
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Data.Int
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import HDF5.HL.Internal.Types
import HDF5.C qualified as C


----------------------------------------------------------------
-- Haskell data type
----------------------------------------------------------------

-- | Size of dimension of dataspace.
data Dim = Dim
  { dimSize    :: !Int64 -- ^ Current size 
  , dimMaxSize :: !Int64 -- ^ Maximum possible size
  }
  deriving stock (Show,Eq,Ord)

-- | Extent of dataspace.
newtype Extent = Extent [Dim]
  deriving stock (Show,Eq,Ord)


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Monoid which is used to fill buffers for calling HDF5 functions.
newtype DSpaceWriter = DSpaceWriter
  (Ptr C.HSize -> Ptr C.HSize -> IO (Ptr C.HSize))

instance Semigroup DSpaceWriter where
  DSpaceWriter f <> DSpaceWriter g = DSpaceWriter $ \grd -> g grd <=< f grd

instance Monoid DSpaceWriter where
  {-# INLINE mempty #-}
  mempty = DSpaceWriter $ \p _ -> pure p

putDimension :: Int -> DSpaceWriter
putDimension (fromIntegral -> i) = DSpaceWriter go where
  go grd ptr
    | ptr >= grd = error "putDimension: buffer overrun"
    | otherwise  = plusPtr ptr (sizeOf i) <$ poke ptr i


-- | Create simple dataspace which could e used in bracket-like
--   context
withDSpace
  :: Int          -- ^ Rank of dataspace
  -> DSpaceWriter -- ^ Size for each dimension
  -> (Dataspace -> IO a)
  -> IO a
withDSpace rank (DSpaceWriter write) action = evalContT $ do
  ptr <- ContT  $ allocaArray rank
  let grd = plusPtr ptr (sz * rank)
  _   <- liftIO $ write grd ptr
  spc <- ContT  $ bracket (C.h5s_create_simple (fromIntegral rank) ptr nullPtr) C.h5s_close
  liftIO $ action $ Dataspace spc
  where
    sz = sizeOf (undefined :: C.HSize) 
