{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -O2 #-}

module SegTree where

import           Data.Function (on)
import           Data.List     (iterate, unfoldr)
import qualified Data.Vector   as V
import           Text.Printf

newtype Size = Size Int
    deriving (Show)
instance Semigroup Size where
    Size a <> Size b = Size$ a+b
instance Monoid Size where
    mempty = Size 0

type Sized v = (Size,v)
sized :: v -> Sized v
sized = (Size 1,)

class (Monoid v,Monoid u) => Update u v where
    appE :: u -> Sized v -> Sized v

data SegTree u v
    = Leaf !v
    | Branch !u {-# UNPACK #-} !(Sized v) (SegTree u v) (SegTree u v)

data SegTreeE v
    = LeafE !v
    | BranchE {-# UNPACK #-} !Int !v (SegTreeE v) (SegTreeE v)

instance (Monoid v) => Update () v where
    appE _ v = v

instance Functor SegTreeE where
    fmap f (LeafE v) = LeafE $ f v
    fmap f (BranchE s v l r) = BranchE s (f v) (fmap f l) (fmap f r)

fmap' :: (Semigroup v') => (v -> v') -> SegTreeE v -> SegTreeE v'
fmap' f (LeafE v) = LeafE $ f v
fmap' f (BranchE s v l r) = BranchE s (evalE l' <> evalE r') l' r'
    where
        l' = fmap' f l
        r' = fmap' f r

renderSeg :: (Show u,Show v) => SegTree u v -> String
renderSeg = renderSeg' [] where
    nodePr []        = []
    nodePr (_:_:prs) = '-':'+':prs

    renderSeg' pr (Leaf v) = printf "%sLeaf v:%s\n" (reverse.nodePr $ pr) (show v)
    renderSeg' pr (Branch u v l r) = printf "%sBranch u:%s v:%s\n%s%s" (reverse.nodePr $ pr) (show u) (show v)
        (renderSeg' (' ':'|':pr) l)
        (renderSeg' (' ':' ':pr) r)

renderSegE :: Show v => SegTreeE v -> String
renderSegE = renderSeg' [] where
    nodePr []        = []
    nodePr (_:_:prs) = '-':'+':prs

    renderSeg' pr (LeafE v) = printf "%sLeaf v:%s\n" (reverse.nodePr $ pr) (show v)
    renderSeg' pr (BranchE i v l r) = printf "%sBranch s:%s v:%s\n%s%s" (reverse.nodePr $ pr) (show i) (show v)
        (renderSeg' (' ':'|':pr) l)
        (renderSeg' (' ':' ':pr) r)

branch :: (Semigroup v,Monoid u) => SegTree u v -> SegTree u v -> SegTree u v
branch l r = Branch mempty (eval l <> eval r) l r

branchE :: (Semigroup v) => SegTreeE v -> SegTreeE v -> SegTreeE v
branchE l r = BranchE (sizeE l + sizeE r) (evalE l <> evalE r) l r

sizeE :: SegTreeE v -> Int
sizeE (LeafE _) = 1
sizeE (BranchE s _ _ _) = s

size :: SegTree u v -> Size
size (Leaf _)           = Size 1
size (Branch _ (s,_) _ _) = s

length :: SegTree u v -> Int
length x = let Size s = size x in s

eval :: SegTree u v -> Sized v
eval (Leaf v)       = (Size 1,v)
eval (Branch u p _ _) = p

evalE :: SegTreeE v -> v
evalE (LeafE v) = v
evalE (BranchE _ v _ _) = v

-- Query SegTree with [li,ri) (0-based index)
query :: (Update u v) => Int -> Int -> SegTree u v -> Sized v
query li ri (Leaf v)
    | li <= 0 && 0 < ri = sized v
    | otherwise = mempty
query li ri (Branch u sv@(Size s,_) l r)
    | li == 0 && ri == s = sv
    | ri <= sl = {-# SCC "query_Left" #-} appE u $! query li ri l
    | sl <= li = {-# SCC "query_Right" #-} appE u $! query (li-sl) (ri-sl) r
    | otherwise = {-# SCC "query_Divide" #-} appE u $! query li sl l <> query 0 (ri-sl) r
    where
        Size sl = size l

queryE :: (Monoid v) => Int -> Int -> SegTreeE v -> v
queryE li ri (LeafE v)
    | li <= 0 && 0 < ri = v
    | otherwise = mempty
queryE li ri (BranchE s v l r)
    | li == 0 && ri == s = v
    | ri <= sl = queryE li ri l
    | sl <= li = queryE (li-sl) (ri-sl) r
    | otherwise = queryE li sl l <> queryE 0 (ri-sl) r
    where
        sl = sizeE l

updateE :: (Monoid v) => Int -> v -> SegTreeE v -> SegTreeE v
updateE i x orig@(LeafE v)
    | i == 0 = LeafE x
    | otherwise = orig
updateE i x orig@(BranchE s v l r)
    | i < sl = updateE i x l `branchE` r
    | sl <= i && i < s = l `branchE` updateE (i-sl) x r
    | otherwise = orig
    where
        sl = sizeE l

-- Update SegTree with [li,ri) (0-based index)
update :: (Update u v) => Int -> Int -> u -> SegTree u v -> SegTree u v
update li ri up orig@(Leaf v)
    | li <= 0 && 0 < ri = Leaf .snd $ appE up $! (sized v)
    | otherwise = orig
update li ri up (Branch u sv@(Size s,_) l r)
    | li == 0 && ri == s = Branch (u <> up) (appE up $! sv) l r
    | ri <= sl = let l' = update li ri up l
        in Branch u  (appE u $! eval l' <> eval r) l' r
    | sl <= li = let r' = update (li-sl) (ri-sl) up r
        in Branch u  (appE u $! eval l <> eval r') l r'
    | otherwise = let   l' = update li sl up l
                        r' = update 0 (ri-sl) up r
        in Branch u (appE u $! eval l'<> eval r') l' r'
    where
        Size sl = size l

fromList :: (Monoid u,Semigroup v) => [v] -> SegTree u v
fromList xs = root where
    leaves = Leaf <$> xs
    ([root]:_) = dropWhile (not.null.tail) $ iterate (unfoldr buildUp) leaves
    buildUp []         = Nothing
    buildUp [x]        = Just (x,[])
    buildUp [x,y,z]    = Just ((x `branch` y) `branch` z,[])
    buildUp (x:y:rest) = Just (x `branch` y,rest)

fromVector :: (Monoid u,Semigroup v) => V.Vector v -> SegTree u v
fromVector vec
    | n == 1    = Leaf $ V.head vec
    | otherwise = (branch `on` fromVector) l r
    where
        n = V.length vec
        (l,r) = V.splitAt (n`div`2) vec

fromVectorE :: (Semigroup v) => V.Vector v -> SegTreeE v
fromVectorE vec
    | n == 1 = LeafE $ V.head vec
    | otherwise = (branchE `on` fromVectorE) l r
    where
        n = V.length vec
        (l,r) = V.splitAt (n`div`2) vec
