{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash           #-}
-- |
-- API for working with HDF5 data types. We treat them as immutable
-- while they're mutable in HDF5.
module HDF5.HL.Internal.TyHDF
  ( -- * Data types
    Type(..)
  , unsafeNewType
  , unsafeNativeType
  , withType
  ) where

import Control.Exception
import Control.Monad
import Data.IORef
import Foreign.Marshal (alloca, allocaArray0)
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import System.IO.Unsafe
import GHC.Exts
import GHC.IO

import HDF5.HL.Internal.CCall
import HDF5.C qualified as C

----------------------------------------------------------------
-- Type definition
----------------------------------------------------------------

-- | HDF5 data type.
data Type
  = Type   C.HID  {-# UNPACK #-} !(IORef ())
    -- ^ Type which should be closed 
  | Native C.HID
    -- ^ Data types which does not need to be finalized

instance Show Type where
  show ty = unsafePerformIO $ withType ty $ \hid -> do
    alloca $ \p_sz -> do
      convertHErr "Can't show type" $ C.h5lt_dtype_to_text hid nullPtr C.h5lt_DDL p_sz
      sz <- peek p_sz
      allocaArray0 (fromIntegral sz) $ \p_str -> do
        convertHErr "Can't show type" $ C.h5lt_dtype_to_text hid p_str C.h5lt_DDL p_sz
        peekCString p_str

-- instance Eq Type where
--   Type h1 _ == Type h2 _ = h1 == h2

-- instance Ord Type where
--   compare (Type h1 _) (Type h2 _) = compare h1 h2
--   Type h1 _ <  Type h2 _ = h1 <  h2
--   Type h1 _ <= Type h2 _ = h1 <= h2
--   Type h1 _ >  Type h2 _ = h1 >  h2
--   Type h1 _ >= Type h2 _ = h1 >= h2

-- | Create new type. IO action must return fresh data type which
--   should be closed with 'C.h5t_close'.
unsafeNewType
  :: IO C.HID -- ^ IO action which generates /fresh/ HID for type
  -> IO Type
unsafeNewType mkHID = mask_ $ do
  token <- newIORef ()
  hid   <- mkHID
  _     <- mkWeakIORef token (void $ C.h5t_close hid)
  pure $ Type hid token 

-- | Create new type. HID must be one of builtin native data types.
unsafeNativeType :: C.HID -> Type
unsafeNativeType = Native

-- | Use HID from data type. This function ensures that HID is kept
--   alive while callback is running.
withType :: Type -> (C.HID -> IO a) -> IO a
withType (Native hid)     fun = fun hid
withType (Type hid token) fun = IO $ \s ->
  case fun hid of
    IO action# -> keepAlive# token s action#


----------------------------------------------------------------
--
----------------------------------------------------------------

