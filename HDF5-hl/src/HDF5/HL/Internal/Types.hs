{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
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
  , tyI8LE, tyI16LE, tyI32LE, tyI64LE
  , tyU8LE, tyU16LE, tyU32LE, tyU64LE
  , tyI8BE, tyI16BE, tyI32BE, tyI64BE
  , tyU8BE, tyU16BE, tyU32BE, tyU64BE
    -- * Patterns
  , pattern Array
  , makePackedRecord
  , makeEnumeration
    -- * Element
  , Element(..)
  , peekElemOffH5
  , peekByteOffH5
  , pokeElemOffH5
  , pokeByteOffH5
  , allocaElement
  , advancePtrH5
  , mallocVectorH5
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Bits                  (finiteBitSize)
import Data.IORef
import Data.Complex
import Data.Proxy
import Data.Int
import Data.Word
import Data.Functor.Identity
import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Unboxed   qualified as FU
import Data.Vector.Fixed.Boxed     qualified as FB
import Data.Vector.Fixed.Storable  qualified as FS
import Data.Vector.Fixed.Primitive qualified as FP
import Foreign.Marshal             (alloca, allocaArray, allocaArray0, withArray, peekArray,
                                    allocaBytesAligned
                                   )
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Storable
import System.IO.Unsafe
import GHC.Exts        (keepAlive#)
import GHC.Stack
import GHC.Generics
import GHC.TypeLits
import GHC.ForeignPtr  (mallocPlainForeignPtrAlignedBytes)
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
    IO action# -> keepAlive# token s action#

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

tyI8LE,tyI16LE,tyI32LE,tyI64LE :: Type
tyI8LE  = Native h5t_STD_I8LE
tyI16LE = Native h5t_STD_I16LE
tyI32LE = Native h5t_STD_I32LE
tyI64LE = Native h5t_STD_I64LE

tyU8LE,tyU16LE,tyU32LE,tyU64LE :: Type
tyU8LE  = Native h5t_STD_U8LE
tyU16LE = Native h5t_STD_U16LE
tyU32LE = Native h5t_STD_U32LE
tyU64LE = Native h5t_STD_U64LE

tyI8BE,tyI16BE,tyI32BE,tyI64BE :: Type
tyI8BE  = Native h5t_STD_I8BE
tyI16BE = Native h5t_STD_I16BE
tyI32BE = Native h5t_STD_I32BE
tyI64BE = Native h5t_STD_I64BE

tyU8BE,tyU16BE,tyU32BE,tyU64BE :: Type
tyU8BE  = Native h5t_STD_U8BE
tyU16BE = Native h5t_STD_U16BE
tyU32BE = Native h5t_STD_U32BE
tyU64BE = Native h5t_STD_U64BE

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

-- | Make enumeration data type which is based on underlying type @a@.
makeEnumeration :: forall a. (Element a, HasCallStack) => [(String,a)] -> Type
makeEnumeration elems = unsafePerformIO $ withFrozenCallStack $ unsafeNewType $ evalContT $ do
  p_err    <- ContT $ alloca
  p_val    <- ContT $ allocaElement
  base_tid <- ContT $ withType (typeH5 @a)
  -- Create enumeration data type. We want to release it in case of
  -- any exception
  tid      <- ContT $ bracketOnError
    ( checkHID p_err "Cannot create enumeration type"
    $ h5t_enum_create base_tid )
    (\tid -> void $ h5t_close tid p_err)
  lift $ do
    forM_ elems $ \(nm,a) -> do
      pokeH5 p_val a
      withCString nm $ \c_nm ->
          checkHErr p_err "Cannot add member to enumeration"
        $ h5t_enum_insert tid c_nm p_val
    pure tid


----------------------------------------------------------------
-- Type class for deriving HDF5 types
----------------------------------------------------------------

-- | Data type which corresponds to some HDF data type and could read
--   from buffer using 'Storable'.
class Element a where
  -- | HDF5 type of element.
  typeH5 :: Type
  -- | Compute size of element. It must be same as @sizeOfH5 (typeH5 \@a)@
  fastSizeOfH5 :: Int
  fastSizeOfH5 = sizeOfH5 (typeH5 @a)

  alignmentH5 :: Int
  -- | Read haskell data type from buffer
  peekH5 :: Ptr a -> IO a
  -- | Write haskell data type into buffer
  pokeH5 :: Ptr a -> a -> IO ()



peekElemOffH5 :: forall a. Element a => Ptr a -> Int -> IO a
{-# INLINE peekElemOffH5 #-}
peekElemOffH5 ptr off = peekByteOffH5 ptr (off * fastSizeOfH5 @a)

pokeElemOffH5 :: forall a. Element a => Ptr a -> Int -> a -> IO ()
{-# INLINE pokeElemOffH5 #-}
pokeElemOffH5 ptr off = pokeByteOffH5 ptr (off * fastSizeOfH5 @a)

peekByteOffH5 :: Element a => Ptr a -> Int -> IO a
{-# INLINE peekByteOffH5 #-}
peekByteOffH5 ptr off = peekH5 (ptr `plusPtr` off)

pokeByteOffH5 :: Element a => Ptr a -> Int -> a -> IO ()
{-# INLINE pokeByteOffH5 #-}
pokeByteOffH5 ptr off = pokeH5 (ptr `plusPtr` off)


allocaElement :: forall a b. Element a => (Ptr a -> IO b) -> IO b
allocaElement = allocaBytesAligned (fastSizeOfH5 @a) (alignmentH5 @a)

advancePtrH5 :: forall a. Element a => Ptr a -> Int -> Ptr a
{-# INLINE advancePtrH5 #-}
advancePtrH5 ptr off = ptr `plusPtr` (off * fastSizeOfH5 @a)

mallocVectorH5 :: forall a. Element a => Int -> IO (ForeignPtr a)
{-# INLINE mallocVectorH5 #-}
mallocVectorH5 size = mallocPlainForeignPtrAlignedBytes
  (size * fastSizeOfH5 @a)
  (alignmentH5 @a)


instance Element Int where
  typeH5 | wordSizeInBits == 64 = tyI64
         | otherwise            = tyI32
  fastSizeOfH5 = sizeOf    (undefined :: Int)
  alignmentH5  = alignment (undefined :: Int)
  peekH5       = peek
  pokeH5       = poke
instance Element Word where
  typeH5 | wordSizeInBits == 64 = tyU64
         | otherwise            = tyU32
  fastSizeOfH5 = sizeOf    (undefined :: Word)
  alignmentH5  = alignment (undefined :: Word)
  peekH5       = peek
  pokeH5       = poke


instance Element Int8   where
  typeH5       = tyI8
  fastSizeOfH5 = sizeOf    (undefined :: Int8)
  alignmentH5  = alignment (undefined :: Int8)
  peekH5       = peek
  pokeH5       = poke
instance Element Int16  where
  typeH5       = tyI16
  fastSizeOfH5 = sizeOf    (undefined :: Int16)
  alignmentH5  = alignment (undefined :: Int16)
  peekH5       = peek
  pokeH5       = poke
instance Element Int32  where
  typeH5       = tyI32
  fastSizeOfH5 = sizeOf    (undefined :: Int32)
  alignmentH5  = alignment (undefined :: Int32)
  peekH5       = peek
  pokeH5       = poke
instance Element Int64  where
  typeH5       = tyI64
  fastSizeOfH5 = sizeOf    (undefined :: Int64)
  alignmentH5  = alignment (undefined :: Int64)
  peekH5       = peek
  pokeH5       = poke
instance Element Word8  where
  typeH5       = tyU8
  fastSizeOfH5 = sizeOf    (undefined :: Word8)
  alignmentH5  = alignment (undefined :: Word8)
  peekH5       = peek
  pokeH5       = poke
instance Element Word16 where
  typeH5       = tyU16
  fastSizeOfH5 = sizeOf    (undefined :: Word16)
  alignmentH5  = alignment (undefined :: Word16)
  peekH5       = peek
  pokeH5       = poke
instance Element Word32 where
  typeH5       = tyU32
  fastSizeOfH5 = sizeOf    (undefined :: Word32)
  alignmentH5  = alignment (undefined :: Word32)
  peekH5       = peek
  pokeH5       = poke
instance Element Word64 where
  typeH5       = tyU64
  fastSizeOfH5 = sizeOf    (undefined :: Word64)
  alignmentH5  = alignment (undefined :: Word64)
  peekH5       = peek
  pokeH5       = poke

instance Element Float  where
  typeH5       = tyF32
  fastSizeOfH5 = sizeOf    (undefined :: Float)
  alignmentH5  = alignment (undefined :: Float)
  peekH5       = peek
  pokeH5       = poke
instance Element Double where
  typeH5       = tyF64
  fastSizeOfH5 = sizeOf    (undefined :: Double)
  alignmentH5  = alignment (undefined :: Double)
  peekH5       = peek
  pokeH5       = poke

-- | Uses same convention as @h5py@ by default.
instance Element a => Element (Complex a) where
  typeH5       = makePackedRecord [("r",ty), ("i",ty)] where ty = typeH5 @a
  fastSizeOfH5 = 2 * fastSizeOfH5 @a
  alignmentH5  = alignmentH5 @a
  peekH5 ptr   = (:+) <$> peekH5 (castPtr ptr) <*> peekElemOffH5 (castPtr ptr) 1
  pokeH5 ptr (re :+ im) = do
    pokeH5        (castPtr ptr)   re
    pokeElemOffH5 (castPtr ptr) 1 im

instance (Element a, F.Arity n) => Element (FB.Vec n a) where
  typeH5       = Array (typeH5 @a) [F.length (undefined :: FB.Vec n a)]
  fastSizeOfH5 = fastSizeOfH5 @a *  F.length (undefined :: FB.Vec n a)
  alignmentH5  = alignmentH5  @a
  peekH5 ptr   = F.generateM (peekElemOffH5 (castPtr ptr))
  pokeH5 ptr v = F.imapM_ (pokeElemOffH5 (castPtr ptr)) v

instance (Element a, F.Arity n, FU.Unbox n a) => Element (FU.Vec n a) where
  typeH5       = Array (typeH5 @a) [F.length (undefined :: FU.Vec n a)]
  fastSizeOfH5 = fastSizeOfH5 @a *  F.length (undefined :: FU.Vec n a)
  alignmentH5  = alignmentH5  @a
  peekH5 ptr   = F.generateM (peekElemOffH5 (castPtr ptr))
  pokeH5 ptr v = F.imapM_ (pokeElemOffH5 (castPtr ptr)) v
instance (Element a, F.Arity n, Storable a) => Element (FS.Vec n a) where
  typeH5       = Array (typeH5 @a) [F.length (undefined :: FS.Vec n a)]
  fastSizeOfH5 = fastSizeOfH5 @a *  F.length (undefined :: FS.Vec n a)
  alignmentH5  = alignmentH5  @a
  peekH5 ptr   = F.generateM (peekElemOffH5 (castPtr ptr))
  pokeH5 ptr v = F.imapM_ (pokeElemOffH5 (castPtr ptr)) v
instance (Element a, F.Arity n, FP.Prim a) => Element (FP.Vec n a) where
  typeH5       = Array (typeH5 @a) [F.length (undefined :: FP.Vec n a)]
  fastSizeOfH5 = fastSizeOfH5 @a *  F.length (undefined :: FP.Vec n a)
  alignmentH5  = alignmentH5  @a
  peekH5 ptr   = F.generateM (peekElemOffH5 (castPtr ptr))
  pokeH5 ptr v = F.imapM_ (pokeElemOffH5 (castPtr ptr)) v

deriving newtype instance Element a => Element (Identity a)

wordSizeInBits :: Int
wordSizeInBits = finiteBitSize (0 :: Word)


#if MIN_VERSION_base(4,17,0)
-- | Derives HDF5 type as record. Uses 'makePackedRecord'
instance ( Generic a
         , GRecElement (Rep a)
         ) => Element (Generically a) where
  typeH5       = makePackedRecord $ gtypeH5 @(Rep a) []
  alignmentH5  = galignmentH5 @(Rep a)
  fastSizeOfH5 = gfastSizeOfH5 @(Rep a)
  peekH5 p = do
    (f,_) <- gpeekH5 (castPtr p) 0
    pure $! Generically $ to f
  pokeH5 p (Generically a) = void $ gpokeH5 (castPtr p) 0 (from a)
  {-# INLINE typeH5       #-}
  {-# INLINE alignmentH5  #-}
  {-# INLINE fastSizeOfH5 #-}
  {-# INLINE peekH5       #-}
  {-# INLINE pokeH5       #-}

class GRecElement f where
  gtypeH5 :: [(String,Type)] -> [(String,Type)]
  gfastSizeOfH5 :: Int
  galignmentH5 :: Int
  gpeekH5 :: Ptr () -> Int -> IO (f p, Int)
  gpokeH5 :: Ptr () -> Int -> f p -> IO Int

deriving newtype instance GRecElement f => GRecElement (M1 D i f)
deriving newtype instance GRecElement f => GRecElement (M1 C i f)

instance (GRecElement f, GRecElement g) => GRecElement (f :*: g) where
  gtypeH5       = gtypeH5 @f . gtypeH5 @g
  gfastSizeOfH5 = gfastSizeOfH5 @f + gfastSizeOfH5 @g
  galignmentH5  = galignmentH5 @f `max` galignmentH5 @g
  gpeekH5 p i = do (f, i')  <- gpeekH5 p i
                   (g, i'') <- gpeekH5 p i'
                   pure (f :*: g, i'')
  gpokeH5 p i (f :*: g) = do i' <- gpokeH5 p i f
                             gpokeH5 p i' g
  {-# INLINE gtypeH5       #-}
  {-# INLINE gfastSizeOfH5 #-}
  {-# INLINE galignmentH5  #-}
  {-# INLINE gpeekH5       #-}
  {-# INLINE gpokeH5       #-}

instance ( KnownSymbol fld
         , Element a
         ) => GRecElement (M1 S (MetaSel (Just fld) u s l) (K1 r a)) where
  gtypeH5 = ((symbolVal (Proxy @fld), typeH5 @a):)
  gfastSizeOfH5 = fastSizeOfH5 @a
  galignmentH5  = alignmentH5  @a
  gpeekH5 p i = do
    a <- peekH5 (castPtr $ plusPtr p i)
    pure (M1 (K1 a), i+len)
    where
      len = fastSizeOfH5 @a
  gpokeH5 p i (M1 (K1 a)) = do
    pokeH5 (castPtr $ plusPtr p i) a
    pure $! i + len
    where
      len = fastSizeOfH5 @a
  {-# INLINE gtypeH5       #-}
  {-# INLINE gfastSizeOfH5 #-}
  {-# INLINE galignmentH5  #-}
  {-# INLINE gpeekH5       #-}
  {-# INLINE gpokeH5       #-}
#endif
