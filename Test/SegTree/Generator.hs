-- module Generator where

import System.Random
import Text.Printf
import Control.Monad
import Data.List

filterPrimes (p:xs) = p:filter (\x -> x`mod`p /=0) xs
primes = filterPrimes [2..]

maxa = 10^9 ::Int
n = 10^5 :: Int
m = 10^6 :: Int

main = do 
    -- n & m
    printf "%d %d\n" n m

    -- Generate init data randomly
    xs <- replicateM n $ randomRIO (2,maxa) 
    putStrLn $ unwords.map show $ xs

    -- Generate operations
    replicateM m $ do
        op <- randomRIO (1,2) :: IO Int
        [l,r] <- sort <$> replicateM 2 (randomRIO (1,n)) :: IO [Int]
        case op of
            1 -> printf "%d %d %d %d\n" op l r =<< randomRIO (2,maxa)
            2 -> printf "%d %d %d\n" op l r
