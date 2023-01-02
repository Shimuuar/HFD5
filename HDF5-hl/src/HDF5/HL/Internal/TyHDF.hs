{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- API for working with HDF5 data types. We treat them as immutable
-- while they're mutable in HDF5.
module HDF5.HL.Internal.TyHDF
  ( -- * Data types
    Type(..)
  , unsafeNewType
  , unsafeNativeType
  , withType
    -- * Scalar data types
  , tyI8, tyI16, tyI32, tyI64
  , tyU8, tyU16, tyU32, tyU64
  , tyF32, tyF64
  , pattern Array
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
import HDF5.C

----------------------------------------------------------------
-- Type definition
----------------------------------------------------------------

-- | HDF5 data type.
data Type
  = Type   HID  {-# UNPACK #-} !(IORef ())
    -- ^ Type which should be closed
  | Native HID
    -- ^ Data types which does not need to be finalized

instance Show Type where
  show ty = unsafePerformIO $ runHIO $ evalContT $ do
    hid   <- ContT $ withType ty
    p_sz  <- liftHIO $ ContT alloca
    lift   $ checkHErr "Can't show type" =<< h5lt_dtype_to_text hid nullPtr h5lt_DDL p_sz
    sz    <- liftIO $ peek p_sz
    p_str <- liftHIO $ ContT $ allocaArray0 (fromIntegral sz)
    lift   $ checkHErr "Can't show type" =<< h5lt_dtype_to_text hid p_str h5lt_DDL p_sz
    liftIO $ peekCString p_str

-- instance Eq Type where
--   Type h1 _ == Type h2 _ = h1 == h2


-- | Create new type. IO action must return fresh data type which
--   should be closed with 'h5t_close'.
unsafeNewType
  :: HIO HID -- ^ IO action which generates /fresh/ HID for type
  -> HIO Type
unsafeNewType mkHID = HIO $ mask_ $ do
  token <- newIORef ()
  hid   <- unHIO mkHID
  _     <- mkWeakIORef token (unHIO $ void $ h5t_close hid)
  pure $ Type hid token

-- | Create new type. HID must be one of builtin native data types.
unsafeNativeType :: HID -> Type
unsafeNativeType = Native

-- | Use HID from data type. This function ensures that HID is kept
--   alive while callback is running.
withType :: Type -> (HID -> HIO a) -> HIO a
withType (Native hid)     fun = fun hid
withType (Type hid token) fun = liftIO $ IO $ \s ->
  case fun hid of
#if MIN_VERSION_base(4,15,0)
    HIO (IO action#) -> keepAlive# token s action#
#else
    HIO (IO action#) -> case action# s of
      (# s', a #) -> let s'' = touch# token s'
                     in (# s'', a #)
#endif

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

pattern Array :: Type -> [Int] -> Type
pattern Array ty dim <- (matchArray -> Just (ty, dim))
  where
    Array ty dim = makeArray ty dim

makeArray :: Type -> [Int] -> Type
makeArray ty dim = unsafePerformIO $ runHIO $ evalContT $ do
  tid   <- ContT $ withType ty
  p_dim <- liftHIO $ ContT $ withArray (fromIntegral <$> dim)
  lift $ unsafeNewType $
    checkHID "Cannot create array type" =<< h5t_array_create tid n p_dim
  where
    n = fromIntegral $ length dim

matchArray :: Type -> Maybe (Type, [Int])
matchArray ty = unsafePerformIO $ runHIO $ withType ty $ \tid -> do
  h5t_get_class tid >>= \case
    H5T_ARRAY -> do
      n <- h5t_get_array_ndims tid
      when (n < 0) $ throwM $ InternalErr "Invalid dimension of an array"
      dims <- liftHIOBracket (allocaArray (fromIntegral n)) $ \buf -> do
        _ <- h5t_get_array_dims tid buf
        liftIO $ peekArray (fromIntegral n) buf
      super <- unsafeNewType $ checkHID "Cannot get supertype" =<< h5t_get_super tid
      pure $ Just (super, fromIntegral <$> dims)
    _ -> pure Nothing
