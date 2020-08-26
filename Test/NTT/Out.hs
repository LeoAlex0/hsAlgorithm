{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -rtsopts #-}
{-# OPTIONS_GHC -with-rtsopts=-N #-}
{-# OPTIONS_GHC -threaded #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
module Main where
import Text.Printf
import Data.Complex
import qualified Data.Vector.Unboxed as V
import Control.Monad
import Control.Monad.ST
import qualified Data.Array as A
import Data.Bits
import Data.Foldable
import Data.Function (fix, on)
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Word

testFFT_Main :: Int -> V.Vector (Complex Double)
testFFT_Main n_Main = fft_NTT $ V.iterateN n_Main (+ 1) 0

testNTT_Main :: Int -> V.Vector ModInt_NTT
testNTT_Main n_Main = fft_NTT $ V.iterateN n_Main (+ 1) 0

maxn_Main :: Int
maxn_Main = 10 ^ 6

main :: IO ()
main
  = do printf "ifft.fft [0..3]=%s\n"
         (show (ifft_NTT $ testFFT_Main 4))
       printf "intt.ntt [0..3]=%s\n" (show (ifft_NTT $ testNTT_Main 4))
       printf "ntt %d:%s\n" maxn_Main
         (show
            ((V.take maxn_Main . ifft_NTT . testNTT_Main) maxn_Main ==
               V.iterateN maxn_Main (+ 1) 0))
       printf "fft %d:%s\n" maxn_Main
         (show
            ((V.take maxn_Main .
                V.map (round . realPart) . ifft_NTT . testFFT_Main)
               maxn_Main
               == (V.iterateN maxn_Main (+ 1) 0 :: V.Vector Int)))

class (Num a) => FFT_NTT a where
        wn_NTT :: Int -> a
        
        inv_NTT :: a -> a

instance (RealFloat a) => FFT_NTT (Complex a) where
        wn_NTT n_NTT = cis $ 2 * pi / fromIntegral n_NTT
        inv_NTT = (1 /)

newtype ModInt_NTT = ModInt_NTT{getI_NTT :: Word32}
                       deriving (Eq, Ord, Show)

instance Enum ModInt_NTT where
        fromEnum = fromEnum . getI_NTT
        toEnum = ModInt_NTT . toEnum

instance Num ModInt_NTT where
        ModInt_NTT x_NTT + ModInt_NTT y_NTT
          = ModInt_NTT $ (x_NTT + y_NTT) `mod` 998244353
        ModInt_NTT x_NTT - ModInt_NTT y_NTT
          = ModInt_NTT $ (x_NTT + 998244353 - y_NTT) `mod` 998244353
        ModInt_NTT x_NTT * ModInt_NTT y_NTT
          = fromIntegral $
              (((*) `on` fromIntegral) x_NTT y_NTT) `mod` 998244353
        abs = id
        signum = const . ModInt_NTT $ 1
        fromInteger x_NTT
          = ModInt_NTT . fromIntegral $ x_NTT `mod` 998244353

instance Real ModInt_NTT where
        toRational = toRational . getI_NTT

instance Integral ModInt_NTT where
        quotRem v1_NTT v2_NTT
          = let (q_NTT, r_NTT) = (quotRem `on` getI_NTT) v1_NTT v2_NTT in
              (ModInt_NTT q_NTT, ModInt_NTT r_NTT)
        toInteger = toInteger . getI_NTT

instance Bounded ModInt_NTT where
        minBound = ModInt_NTT 0
        maxBound = fromInteger (-1)

instance FFT_NTT ModInt_NTT where
        wn_NTT n_NTT
          | n_NTT < 0 = inv_NTT $ wn_NTT (-n_NTT)
          | otherwise = ModInt_NTT 3 ^ (998244352 `div` n_NTT)
        inv_NTT x_NTT = x_NTT ^ 998244351

newtype instance 
        M.MVector s ModInt_NTT = MV__ModInt_NTT (M.MVector s Word32)

newtype instance  V.Vector ModInt_NTT = V__ModInt_NTT (V.Vector
                                                         Word32)

instance V.Unbox ModInt_NTT

instance GM.MVector M.MVector ModInt_NTT where
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
        basicLength (MV__ModInt_NTT v_NTT) = GM.basicLength v_NTT
        basicUnsafeSlice i_NTT n_NTT (MV__ModInt_NTT v_NTT)
          = MV__ModInt_NTT $ GM.basicUnsafeSlice i_NTT n_NTT v_NTT
        basicOverlaps (MV__ModInt_NTT v1_NTT) (MV__ModInt_NTT v2_NTT)
          = GM.basicOverlaps v1_NTT v2_NTT
        basicUnsafeNew n_NTT
          = MV__ModInt_NTT `liftM` GM.basicUnsafeNew n_NTT
        basicInitialize (MV__ModInt_NTT v_NTT) = GM.basicInitialize v_NTT
        basicUnsafeReplicate n_NTT (ModInt_NTT x_NTT)
          = MV__ModInt_NTT `liftM` GM.basicUnsafeReplicate n_NTT x_NTT
        basicUnsafeRead (MV__ModInt_NTT v_NTT) i_NTT
          = ModInt_NTT `liftM` GM.basicUnsafeRead v_NTT i_NTT
        basicUnsafeWrite (MV__ModInt_NTT v_NTT) i_NTT (ModInt_NTT x_NTT)
          = GM.basicUnsafeWrite v_NTT i_NTT x_NTT
        basicClear (MV__ModInt_NTT v_NTT) = GM.basicClear v_NTT
        basicSet (MV__ModInt_NTT v_NTT) (ModInt_NTT x_NTT)
          = GM.basicSet v_NTT x_NTT
        basicUnsafeCopy (MV__ModInt_NTT v1_NTT) (MV__ModInt_NTT v2_NTT)
          = GM.basicUnsafeCopy v1_NTT v2_NTT
        basicUnsafeMove (MV__ModInt_NTT v1_NTT) (MV__ModInt_NTT v2_NTT)
          = GM.basicUnsafeMove v1_NTT v2_NTT
        basicUnsafeGrow (MV__ModInt_NTT v_NTT) n_NTT
          = MV__ModInt_NTT `liftM` GM.basicUnsafeGrow v_NTT n_NTT

instance G.Vector V.Vector ModInt_NTT where
        {-# INLINE basicUnsafeFreeze #-}
        
        {-# INLINE basicUnsafeThaw #-}
        
        {-# INLINE basicLength #-}
        
        {-# INLINE basicUnsafeSlice #-}
        
        {-# INLINE basicUnsafeIndexM #-}
        
        {-# INLINE elemseq #-}
        basicUnsafeFreeze (MV__ModInt_NTT v_NTT)
          = V__ModInt_NTT `liftM` G.basicUnsafeFreeze v_NTT
        basicUnsafeThaw (V__ModInt_NTT v_NTT)
          = MV__ModInt_NTT `liftM` G.basicUnsafeThaw v_NTT
        basicLength (V__ModInt_NTT v_NTT) = G.basicLength v_NTT
        basicUnsafeSlice i_NTT n_NTT (V__ModInt_NTT v_NTT)
          = V__ModInt_NTT $ G.basicUnsafeSlice i_NTT n_NTT v_NTT
        basicUnsafeIndexM (V__ModInt_NTT v_NTT) i_NTT
          = ModInt_NTT `liftM` G.basicUnsafeIndexM v_NTT i_NTT
        basicUnsafeCopy (MV__ModInt_NTT mv_NTT) (V__ModInt_NTT v_NTT)
          = G.basicUnsafeCopy mv_NTT v_NTT
        elemseq _ (ModInt_NTT x_NTT) z_NTT
          = G.elemseq (undefined :: V.Vector a) x_NTT z_NTT
memoArr_NTT r_NTT f_NTT
  = (A.!) $ A.listArray r_NTT $ f_NTT <$> A.range r_NTT
rev'_NTT p_NTT f_NTT 0 = 0
rev'_NTT p_NTT f_NTT i_NTT
  = (i_NTT .&. 1) `shiftL` (p_NTT - 1) .|. f_NTT (i_NTT `shiftR` 1)
      `shiftR` 1

rev_NTT :: Int -> Int -> Int
rev_NTT p_NTT
  = fix (memoArr_NTT (0, 2 ^ p_NTT - 1) . rev'_NTT p_NTT)
fftPre_NTT v_NTT
  = do let n_NTT = V.length v_NTT
           p_NTT = ceiling . logBase 2 . fromIntegral $ n_NTT
           l_NTT = 2 ^ p_NTT
       vs'_NTT <- M.new l_NTT
       M.set vs'_NTT 0
       V.copy (M.slice 0 n_NTT vs'_NTT) v_NTT
       fftBtf_NTT vs'_NTT
       pure vs'_NTT
fftBtf_NTT vs_NTT
  = do let n_NTT = M.length vs_NTT
           p_NTT = ceiling . logBase 2 . fromIntegral $ n_NTT
           revN_NTT = rev_NTT p_NTT
       [0 .. n_NTT - 1] `for_`
         (\ i_NTT ->
            do let bi_NTT = revN_NTT i_NTT
               if i_NTT < bi_NTT then M.swap vs_NTT i_NTT bi_NTT else pure ())
fftMain_NTT wt_NTT vs_NTT
  = do let n_NTT = M.length vs_NTT
       if n_NTT == 1 then pure () else
         do let (l_NTT, r_NTT) = M.splitAt mid_NTT vs_NTT
                mid_NTT = n_NTT `div` 2
                wt2_NTT = wt_NTT ^ 2
            fftMain_NTT wt2_NTT l_NTT
            fftMain_NTT wt2_NTT r_NTT
            [0 .. mid_NTT - 1] `for_`
              (\ i_NTT ->
                 do lx_NTT <- M.read l_NTT i_NTT
                    rx_NTT <- (wt_NTT ^ i_NTT *) <$> M.read r_NTT i_NTT
                    M.write l_NTT i_NTT (lx_NTT + rx_NTT)
                    M.write r_NTT i_NTT (lx_NTT - rx_NTT))
fftBase_NTT r_NTT vs_NTT
  = do let n_NTT = M.length vs_NTT
       fftMain_NTT (wn_NTT (r_NTT * n_NTT)) vs_NTT
fftIn'_NTT r_NTT mv_NTT
  = do fftBtf_NTT mv_NTT
       fftBase_NTT r_NTT mv_NTT
fftIn_NTT = fftIn'_NTT 1
ifftIn_NTT sv_NTT
  = do let n_NTT = M.length sv_NTT
           invn_NTT = inv_NTT . fromIntegral $ n_NTT
       fftIn'_NTT (-1) sv_NTT
       [0 .. n_NTT - 1] `for_` M.modify sv_NTT (* invn_NTT)
fft_NTT v_NTT
  = runST $
      do vs_NTT <- fftPre_NTT v_NTT
         fftBase_NTT 1 vs_NTT
         V.unsafeFreeze vs_NTT
ifft_NTT v_NTT
  = runST $
      do vs_NTT <- fftPre_NTT v_NTT
         fftBase_NTT (-1) vs_NTT
         let n_NTT = M.length vs_NTT
             invn_NTT = inv_NTT . fromIntegral $ n_NTT
         [0 .. n_NTT - 1] `for_` M.modify vs_NTT (* invn_NTT)
         V.unsafeFreeze vs_NTT

conv_NTT ::
           (FFT_NTT a, V.Unbox a) => V.Vector a -> V.Vector a -> V.Vector a
conv_NTT a_NTT b_NTT
  = ((ifft_NTT .) . (V.zipWith (*) `on` fft_NTT)) a'_NTT b'_NTT
  where [na_NTT, nb_NTT] = V.length <$> [a_NTT, b_NTT]
        n'_NTT = na_NTT + nb_NTT - 1
        [a'_NTT, b'_NTT] = padding_NTT n'_NTT <$> [a_NTT, b_NTT]

padding_NTT ::
              (Num a, V.Unbox a) => Int -> V.Vector a -> V.Vector a
padding_NTT n_NTT x_NTT
  = x_NTT V.++ V.replicate (max 0 (n_NTT - len_NTT)) 0
  where len_NTT = V.length x_NTT

