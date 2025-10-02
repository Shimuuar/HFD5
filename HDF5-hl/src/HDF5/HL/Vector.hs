{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Analog of storable vector which uses 'Element' type class for
-- encoding values in a buffer
module HDF5.HL.Vector
  ( MVecHDF5
  , VecHDF5
  , unsafeFromForeignPtr
  , unsafeWithH5
  ) where

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Utils (fillBytes, copyBytes, moveBytes)
import GHC.Ptr               (Ptr(..))
import GHC.ForeignPtr        (ForeignPtr(..), unsafeWithForeignPtr)
import GHC.Exts              (IsList(..))

import Control.DeepSeq             (NFData(..), NFData1(..))
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Semigroup
import Data.Vector.Fusion.Bundle   qualified as Bundle
import Data.Vector.Generic         qualified as VG
import Data.Vector.Generic.Mutable qualified as MVG
import Text.Read

import HDF5.HL.Internal.Types


type role MVecHDF5 nominal nominal
type role VecHDF5  nominal

-- | Mutable 'Element'-based vectors.
data MVecHDF5 s a = MVecHDF5 {-# UNPACK #-} !Int
                             {-# UNPACK #-} !(ForeignPtr a)

-- | Immutable 'Element'-based vectors.
data VecHDF5 a = VecHDF5 {-# UNPACK #-} !Int
                         {-# UNPACK #-} !(ForeignPtr a)

type instance VG.Mutable VecHDF5 = MVecHDF5


instance Element a => MVG.MVector MVecHDF5 a where
  {-# INLINE basicLength #-}
  basicLength (MVecHDF5 n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (MVecHDF5 _ fp) = MVecHDF5 m (updPtr (`advancePtrH5` j) fp)

  -- FIXME: this relies on non-portable pointer comparisons
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVecHDF5 m fp) (MVecHDF5 n fq)
    = between p q (q `advancePtrH5` n) || between q p (p `advancePtrH5` m)
    where
      between x y z = x >= y && x < z
      p = getPtr fp
      q = getPtr fq

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n
    | n < 0 = error $ "Storable.basicUnsafeNew: negative length: " ++ show n
    | n > mx = error $ "Storable.basicUnsafeNew: length too large: " ++ show n
    | otherwise = unsafePrimToPrim $ do
        fp <- mallocVectorH5 n
        return $ MVecHDF5 n fp
    where
      size = fastSizeOfH5 @a `max` 1
      mx = maxBound `quot` size :: Int

  {-# INLINE basicInitialize #-}
  basicInitialize = storableZero

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVecHDF5 _ fp) i
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp (`peekElemOffH5` i)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVecHDF5 _ fp) i x
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p -> pokeElemOffH5 p i x

  -- FIXME: It would be nice to have optimized version but default
  --        implementation is goood enough for now
  --
  -- {-# INLINE basicSet #-}
  -- basicSet = elementSet

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVecHDF5 n fp) (MVecHDF5 _ fq)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p ->
      unsafeWithForeignPtr fq $ \q ->
      copyArray p q n

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVecHDF5 n fp) (MVecHDF5 _ fq)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p ->
      unsafeWithForeignPtr fq $ \q ->
      moveArray p q n


instance Element a => VG.Vector VecHDF5 a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVecHDF5 n fp) = return $ VecHDF5 n fp

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (VecHDF5 n fp) = return $ MVecHDF5 n fp

  {-# INLINE basicLength #-}
  basicLength (VecHDF5 n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (VecHDF5 _ fp) = VecHDF5 n (updPtr (`advancePtrH5` i) fp)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (VecHDF5 _ fp) i = return
                                     . unsafeInlineIO
                                     $ unsafeWithForeignPtr fp $ \p ->
                                       peekElemOffH5 p i

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVecHDF5 n fp) (VecHDF5 _ fq)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p ->
      unsafeWithForeignPtr fq $ \q ->
      copyArray p q n

  {-# INLINE elemseq #-}
  elemseq _ = seq


instance (Element a, Eq a) => Eq (VecHDF5 a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (VG.stream xs) (VG.stream ys)

instance (Element a, Ord a) => Ord (VecHDF5 a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (VG.stream xs) (VG.stream ys)
  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (VG.stream xs) (VG.stream ys) == LT
  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (VG.stream xs) (VG.stream ys) /= GT
  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (VG.stream xs) (VG.stream ys) == GT
  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (VG.stream xs) (VG.stream ys) /= LT


instance Element a => Semigroup (VecHDF5 a) where
  {-# INLINE (<>) #-}
  (<>) = (VG.++)
  {-# INLINE sconcat #-}
  sconcat = VG.concatNE

instance Element a => Monoid (VecHDF5 a) where
  {-# INLINE mempty #-}
  mempty = VG.empty
  {-# INLINE mconcat #-}
  mconcat = VG.concat

instance Element a => IsList (VecHDF5 a) where
  type Item (VecHDF5 a) = a
  fromList = VG.fromList
  fromListN = VG.fromListN
  toList = VG.toList

instance NFData (VecHDF5 a) where
  rnf (VecHDF5 _ _) = ()

instance NFData1 VecHDF5 where
  liftRnf _ (VecHDF5 _ _) = ()

instance (Show a, Element a) => Show (VecHDF5 a) where
  showsPrec = VG.showsPrec

instance (Read a, Element a) => Read (VecHDF5 a) where
  readPrec = VG.readPrec
  readListPrec = readListPrecDefault


unsafeFromForeignPtr :: ForeignPtr a -> Int -> VecHDF5 a
{-# INLINE unsafeFromForeignPtr #-}
unsafeFromForeignPtr ptr n = VecHDF5 n ptr

unsafeWithH5 :: VecHDF5 a -> (Ptr a -> IO b) -> IO b
{-# INLINE unsafeWithH5 #-}
unsafeWithH5 (VecHDF5 _ fp) = unsafeWithForeignPtr fp 

----------------------------------------------------------------
-- Helpers (copied from vector)
----------------------------------------------------------------

updPtr :: (Ptr a -> Ptr a) -> ForeignPtr a -> ForeignPtr a
{-# INLINE updPtr #-}
updPtr f (ForeignPtr p c) = case f (Ptr p) of { Ptr q -> ForeignPtr q c }

getPtr :: ForeignPtr a -> Ptr a
{-# INLINE getPtr #-}
getPtr (ForeignPtr addr _) = Ptr addr


storableZero :: forall a s. (Element a) => MVecHDF5 s a -> ST s ()
{-# INLINE storableZero #-}
storableZero (MVecHDF5 n fp)
  = unsafePrimToPrim . unsafeWithForeignPtr fp $ \ptr->
      fillBytes ptr 0 byteSize
  where
    byteSize = n * fastSizeOfH5 @a


copyArray :: forall a . Element a => Ptr a -> Ptr a -> Int -> IO ()
{-# INLINE copyArray #-}
copyArray dest src size = copyBytes dest src (size * fastSizeOfH5 @a)


moveArray :: forall a . Element a => Ptr a -> Ptr a -> Int -> IO ()
{-# INLINE moveArray #-}
moveArray dest src size = moveBytes dest src (size * fastSizeOfH5 @a)
