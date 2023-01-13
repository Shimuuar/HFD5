{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- API for working with HDF5 data types. We treat them as immutable
-- while they're mutable in HDF5.
module HDF5.HL.Internal.TyHDF
  ( -- * Operations on types
    basicShowType
    -- * Scalar data types
  , tyI8, tyI16, tyI32, tyI64
  , tyU8, tyU16, tyU32, tyU64
  , tyF32, tyF64
    -- * Arrays
  , makeArray
  -- , matchArrat
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.IORef
import Foreign.Marshal (alloca, allocaArray, allocaArray0, withArray, peekArray)
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import System.IO.Unsafe
import GHC.Exts
import GHC.IO          (IO(..))

import HDF5.HL.Internal.Error
import HDF5.HL.Internal.Types
import HDF5.C

----------------------------------------------------------------
-- Type definition
----------------------------------------------------------------

basicShowType :: Type -> IO String
basicShowType (getHID -> tid) = evalContT $ do
  p_sz  <- ContT $ alloca
  _     <- lift  $ checkHErr "Can't show type"
                 $ h5lt_dtype_to_text tid nullPtr h5lt_DDL p_sz
  sz    <- lift  $ peek p_sz
  p_str <- ContT $ allocaArray0 $ fromIntegral sz
  lift   $ do
    checkHErr "Can't show type" $ h5lt_dtype_to_text tid p_str h5lt_DDL p_sz
    peekCString p_str

----------------------------------------------------------------
--
----------------------------------------------------------------

tyI8,tyI16,tyI32,tyI64 :: Type
tyI8  = Native h5t_NATIVE_SCHAR
tyI16 = Native h5t_NATIVE_SHORT
tyI32 = Native h5t_NATIVE_INT
tyI64 = Native h5t_NATIVE_LONG

tyU8,tyU16,tyU32,tyU64 :: Type
tyU8  = Native h5t_NATIVE_UCHAR
tyU16 = Native h5t_NATIVE_USHORT
tyU32 = Native h5t_NATIVE_UINT
tyU64 = Native h5t_NATIVE_ULONG

tyF32,tyF64 :: Type
tyF32 = Native h5t_NATIVE_FLOAT
tyF64 = Native h5t_NATIVE_DOUBLE

-- pattern Array :: Type -> [Int] -> Type
-- pattern Array ty dim <- (matchArray -> Just (ty, dim))
--   where
--     Array ty dim = makeArray ty dim

makeArray :: Type -> [Int] -> IO Type
makeArray (getHID -> tid) dim =
  withArray (fromIntegral <$> dim) $ \p_dim -> do
    arr <- checkHID "Cannot create array type" $ h5t_array_create tid n p_dim
    pure $ Type arr
  where
    n = fromIntegral $ length dim

-- matchArray :: Type -> Maybe (Type, [Int])
-- matchArray ty = undefined
-- unsafePerformIO $ runHIO $ withType ty $ \tid -> do
--   h5t_get_class tid >>= \case
--     H5T_ARRAY -> evalContT $ do
--       n <- lift $ fromIntegral <$> h5t_get_array_ndims tid
--       when (n < 0) $ throwM $ InternalErr "Invalid dimension of an array"
--       buf  <- hioAllocaArrayC n
--       lift $ do
--         _     <- h5t_get_array_dims tid buf
--         dims  <- hioPeekArray n buf
--         super <- unsafeNewType $ checkHID "Cannot get supertype" =<< h5t_get_super tid
--         pure $ Just (super, fromIntegral <$> dims)
--     _ -> pure Nothing
