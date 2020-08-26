{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -O2 #-}
module Main (main) where
import SegTree
import Control.Monad
import Data.IORef
import Control.Applicative
import Data.Functor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import System.IO
import Data.Maybe

data GcdNE = GcdNE { fromGcd :: {-# UNPACK #-} !Int } deriving (Show)
instance Semigroup GcdNE where
    GcdNE a <> GcdNE b = GcdNE $ gcd a b
type Gcd = Maybe GcdNE
gcdD = Just .GcdNE

newtype AssignNE a = Assign a deriving (Show)
instance Semigroup (AssignNE a) where
    (<>) = flip const
type Assign a = Maybe (AssignNE a)
assign = Just .Assign

instance Update (Assign Int) Gcd where
    appE Nothing x = x
    appE (Just (Assign x)) (s,_) = (s,).gcdD $ x

main :: IO ()
main = do
    [n,m] <- fmap int.BC.words <$> BS.getLine
    tr <- newIORef =<< fromVector.V.fromList.fmap (gcdD .int).BC.words <$> BS.getLine
    replicateM_ m $ do
        op <- map int.BC.words <$> BS.getLine
        case op of
            [1,x,y,k] -> modifyIORef' tr $ update (x-1) y (assign k)
            [2,x,y] -> print.fromGcd.fromJust.snd.query (x-1) y =<< readIORef tr
            _ -> print "Unknown Command"

int :: BC.ByteString -> Int
int = fst.fromJust.BC.readInt
