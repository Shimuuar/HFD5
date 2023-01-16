{-# LANGUAGE BangPatterns        #-}
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
module HDF5.HL.Internal.Types
  ( -- * Operations on types
    Type(..)
  , unsafeNewType
  , withType
  , sizeOfH5
    -- * Scalar data types
  , tyI8, tyI16, tyI32, tyI64
  , tyU8, tyU16, tyU32, tyU64
  , tyF32, tyF64
    -- * Patterns
  , pattern Array
  , makePackedRecord
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
import GHC.Stack
import GHC.IO          (IO(..))

import HDF5.HL.Internal.Error
-- import HDF5.HL.Internal.Types
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

-- | Create new type. IO action must return fresh data type which
--   should be closed with 'h5t_close'.
unsafeNewType
  :: IO HID -- ^ IO action which generates /fresh/ HID for type
  -> IO Type
unsafeNewType mkHID = alloca $ \p_err -> mask_ $ do
  token <- newIORef ()
  hid   <- mkHID
  _     <- mkWeakIORef token (void $ h5t_close hid p_err)
  pure $ Type hid token

-- | Use HID from data type. This function ensures that HID is kept
--   alive while callback is running.
withType :: Type -> (HID -> IO a) -> IO a
withType (Native hid)     fun = fun hid
withType (Type hid token) fun = IO $ \s ->
  case fun hid of
#if MIN_VERSION_base(4,15,0)
    IO action# -> keepAlive# token s action#
#else
    IO action# -> case action# s of
      (# s', a #) -> let s'' = touch# token s'
                     in (# s'', a #)
#endif

instance Show Type where
  show ty = unsafePerformIO $ evalContT $ do
    p_err <- ContT $ alloca
    tid   <- ContT $ withType ty
    p_sz  <- ContT $ alloca
    _     <- lift  $ checkHErr p_err "Can't show type"
                   $ h5lt_dtype_to_text tid nullPtr h5lt_DDL p_sz
    sz    <- lift  $ peek p_sz
    p_str <- ContT $ allocaArray0 $ fromIntegral sz
    lift $ do
      checkHErr p_err "Can't show type" $ h5lt_dtype_to_text tid p_str h5lt_DDL p_sz
      peekCString p_str


sizeOfH5 :: HasCallStack => Type -> Int
sizeOfH5 ty = withFrozenCallStack $ unsafePerformIO $
  withType ty $ \tid -> do
    alloca $ \p_err -> do
      sz <- h5t_get_size tid p_err
      case sz of
        0 -> throwM =<< decodeError p_err "Cannot compute size of data type"
        _ -> pure $! fromIntegral sz


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
makeArray ty dim = unsafePerformIO $ evalContT $ do
  tid   <- ContT $ withType ty
  p_dim <- ContT $ withArray (fromIntegral <$> dim)
  p_err <- ContT $ alloca
  liftIO $ unsafeNewType
         $ checkHID p_err "Cannot create array type"
         $ h5t_array_create tid n p_dim
  where
    n = fromIntegral $ length dim

matchArray :: Type -> Maybe (Type, [Int])
matchArray ty = unsafePerformIO $ evalContT $ do
  p_err <- ContT $ alloca
  tid   <- ContT $ withType ty
  liftIO (h5t_get_class tid p_err) >>= \case
    H5T_NO_CLASS -> liftIO $ throwM =<< decodeError p_err "INTERNAL: Unable to get class for a type"
    H5T_ARRAY    -> do
        n     <- liftIO
               $ fmap fromIntegral
               $ checkCInt p_err "INTERNAL: Unable to get number of array dimensions"
               $ h5t_get_array_ndims tid
        buf   <- ContT $ allocaArray n
        _     <- liftIO
               $ checkCInt p_err "INTERNAL: Unable to get array's dimensions"
               $ h5t_get_array_dims tid buf
        super <- liftIO
               $ unsafeNewType
               $ checkHID p_err "INTERNAL: Cannot get supertype"
               $ h5t_get_super tid
        dims  <- liftIO $ peekArray n buf
        pure $ Just (super, fromIntegral <$> dims)
    _ -> pure Nothing


makePackedRecord :: HasCallStack => [(String,Type)] -> Type
makePackedRecord fields = unsafePerformIO $ withFrozenCallStack $ unsafeNewType $ do
  alloca $ \p_err -> do
    ty_rec <- checkHID p_err "Cannot create compound type"
            $ h5t_create H5T_COMPOUND (fromIntegral size)
    -- NOTE: At this point we can't simply throw exceptions or else
    --       we'll leak ty_rec.
    forM_ fields_sz $ \(nm,ty,off) -> do
      withCString nm $ \c_nm -> do
        withType ty $ \tid ->
          h5t_insert ty_rec c_nm (fromIntegral off) tid p_err >>= \case
            -- We must call decodeError before h5t_close in order to
            -- recover stack. Also we closing ty_rec on best effort
            -- basis
            HErrored -> do err <- decodeError p_err "Unable to pack data types"
                           _   <- h5t_close ty_rec p_err
                           throwM err
            _        -> pure ()
    pure ty_rec
  where
    computeOff !off [] = (off,[])
    computeOff !off ((nm,ty):rest) = (sz, (nm,ty,off) : rest') where
      (sz,rest') = computeOff (off + sizeOfH5 ty) rest
    (size, fields_sz) = computeOff 0 fields
