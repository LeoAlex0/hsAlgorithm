{-# OPTIONS_GHC -O2 #-}
import NTT
import Text.Printf
import Data.Complex
import qualified Data.Vector.Unboxed as V

testFFT :: Int -> V.Vector (Complex Double)
testFFT n = fft $ V.iterateN n (+1) 0

testNTT :: Int -> V.Vector ModInt
testNTT n = fft $ V.iterateN n (+1) 0

maxn :: Int
maxn = 10^6

main :: IO ()
main = do
    printf "ifft.fft [0..3]=%s\n" (show (ifft $ testFFT 4))
    printf "intt.ntt [0..3]=%s\n" (show (ifft $ testNTT 4))
    printf "ntt %d:%s\n" maxn
        (show ((V.take maxn.ifft.testNTT) maxn == V.iterateN maxn (+1) 0))
    printf "fft %d:%s\n" maxn
        (show ((V.take maxn.V.map (round.realPart).ifft.testFFT) maxn == (V.iterateN maxn (+1) 0:: V.Vector Int)))
    
