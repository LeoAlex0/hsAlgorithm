{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -O2       #-}
module NTT (fft,ifft,fftIn,ifftIn,ModInt) where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.Array          as A
import           Data.Bits
import           Data.Complex
import           Data.Foldable
import           Data.Function       (fix, on)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import           Data.Word

class (Num a) => FFT a where
    {- satisfy (wn n)^n     = fromIntegral 1    -}
    {- satisfy (wn n)^(n/2) = fromIntegral (-1) -}
    wn :: Int -> a
    {- satisfy inv x*x = x*inv x = x -}
    inv :: a -> a

instance (RealFloat a) => FFT (Complex a) where
    wn n = cis $ 2*pi/fromIntegral n
    inv = (1/)

newtype ModInt = ModInt { getI::Word32 }
    deriving (Eq,Ord,Show)

instance Enum ModInt where
    fromEnum = fromEnum.getI
    toEnum = ModInt. toEnum

instance Num ModInt where
    ModInt x + ModInt y = ModInt $ (x + y) `mod` 998244353
    ModInt x - ModInt y = ModInt $ (x + 998244353 - y) `mod` 998244353
    ModInt x * ModInt y = fromIntegral $ (((*) `on` fromIntegral) x y) `mod` 998244353
    abs = id
    signum = const.ModInt $ 1
    fromInteger x = ModInt .fromIntegral $ x `mod` 998244353

instance Real ModInt where
    toRational = toRational.getI

instance Integral ModInt where
    quotRem v1 v2 = let (q,r) = (quotRem `on` getI) v1 v2 in (ModInt q,ModInt r)
    toInteger = toInteger.getI

instance Bounded ModInt where
    minBound = ModInt 0
    maxBound = fromInteger (-1)

instance FFT ModInt where
    wn n
        | n < 0 = inv $ wn (-n)
        | otherwise = ModInt 3^(998244352 `div` n)
    inv x = x^998244351

newtype instance M.MVector s ModInt = MV_ModInt (M.MVector s Word32)
newtype instance V.Vector    ModInt = V_ModInt  (V.Vector    Word32)

instance V.Unbox ModInt

instance GM.MVector M.MVector ModInt where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_ModInt v) = GM.basicLength v
  basicUnsafeSlice i n (MV_ModInt v) = MV_ModInt $ GM.basicUnsafeSlice i n v
  basicOverlaps (MV_ModInt v1) (MV_ModInt v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew n = MV_ModInt `liftM` GM.basicUnsafeNew n
  basicInitialize (MV_ModInt v) = GM.basicInitialize v
  basicUnsafeReplicate n (ModInt x) = MV_ModInt `liftM` GM.basicUnsafeReplicate n x
  basicUnsafeRead (MV_ModInt v) i = ModInt `liftM` GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_ModInt v) i (ModInt x) = GM.basicUnsafeWrite v i x
  basicClear (MV_ModInt v) = GM.basicClear v
  basicSet (MV_ModInt v) (ModInt x) = GM.basicSet v x
  basicUnsafeCopy (MV_ModInt v1) (MV_ModInt v2) = GM.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_ModInt v1) (MV_ModInt v2) = GM.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_ModInt v) n = MV_ModInt `liftM` GM.basicUnsafeGrow v n

instance G.Vector V.Vector ModInt where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_ModInt v) = V_ModInt `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_ModInt v) = MV_ModInt `liftM` G.basicUnsafeThaw v
  basicLength (V_ModInt v) = G.basicLength v
  basicUnsafeSlice i n (V_ModInt v) = V_ModInt $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_ModInt v) i = ModInt `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_ModInt mv) (V_ModInt v)
                = G.basicUnsafeCopy mv v
  elemseq _ (ModInt x) z = G.elemseq (undefined :: V.Vector a) x z

memoArr r f = (A.!) $ A.listArray r $ f <$> A.range r
rev' p f 0 = 0
rev' p f i = (i .&. 1) `shiftL` (p-1) .|. f (i `shiftR` 1) `shiftR` 1
rev :: Int -> Int -> Int
rev p = fix (memoArr (0,2^p-1).rev' p)

-- fftPre :: V.Vector (Complex Double) -> ST s (M.STVector s (Complex Double))
fftPre v = do
    let n = V.length v
        p = ceiling.logBase 2.fromIntegral $ n
        l = 2^p
    vs' <- M.new l
    M.set vs' 0
    V.copy (M.slice 0 n vs') v
    fftBtf vs'
    pure vs'

fftBtf vs = do
    let n = M.length vs
        p = ceiling.logBase 2.fromIntegral $ n
        revN = rev p
    [0..n-1] `for_` (\i -> do
        let bi = revN i
        if i < bi then M.swap vs i bi else pure ()
        )


-- fft' :: Complex Double -> M.STVector s (Complex Double) -> ST s ()
fftMain wt vs = do
    let n = M.length vs
    if n == 1 then pure ()
    else do
        let (l,r) = M.splitAt mid vs
            mid = n `div` 2
            wt2 = wt^2
        fftMain wt2 l
        fftMain wt2 r
        [0..mid-1] `for_` (\i -> do
            lx <- M.read l i
            rx <- (wt^i*) <$> M.read r i
            M.write l i (lx+rx)
            M.write r i (lx-rx)
            )

fftBase r vs = do
    let n = M.length vs
    fftMain (wn (r*n)) vs

fftIn' r mv = do
    fftBtf mv
    fftBase r mv

fftIn = fftIn' 1
ifftIn sv = do
    let n = M.length sv
        invn = inv.fromIntegral $ n
    fftIn' (-1) sv
    [0..n-1] `for_` M.modify sv (*invn)

fft v = runST $ do
    vs <- fftPre v
    fftBase 1 vs
    V.unsafeFreeze vs

ifft v = runST $ do
    vs <- fftPre v
    fftBase (-1) vs
    let n = M.length vs
        invn = inv.fromIntegral $ n
    [0..n-1] `for_` M.modify vs (*invn)
    V.unsafeFreeze vs

conv :: (FFT a,V.Unbox a) => V.Vector a -> V.Vector a -> V.Vector a
conv a b = ((ifft.).(V.zipWith (*) `on` fft)) a' b' where
    [na,nb] = V.length <$> [a,b]
    n' = na+nb-1
    [a',b'] = padding n' <$> [a,b]

padding :: (Num a,V.Unbox a) => Int -> V.Vector a -> V.Vector a
padding n x = x V.++ V.replicate (max 0 (n-len)) 0 where
    len = V.length x

{-# SPECIALIZE INLINE fft::V.Vector (Complex Double) -> V.Vector (Complex Double) #-}
{-# SPECIALIZE INLINE fft::V.Vector ModInt -> V.Vector ModInt #-}
