--  !!  ATTENTION: SPOILERS AHEAD  !!  --

module BinaryTrees where

import Prelude hiding ( Left, Right, flip, insert )

import Data.Function  ( on )
import Data.List      ( nubBy )
import Control.Monad  ( void )

import Data.Composition ( (.:) )

import Lists          ( combinations )

{-# ANN module "HLint: ignore Use fmap"    #-}
{-# ANN module "HLint: ignore Use mappend" #-}

data Tree a = Empty | Branch { label :: a, left :: Tree a, right :: Tree a }
              deriving (Show, Eq)

-- Utilities:
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

height :: Tree a -> Int
height  Empty         = 0
height (Branch _ l r) = 1 + max (height l) (height r)

flip :: Tree a -> Tree a
flip  Empty         = Empty
flip (Branch x l r) = Branch x r l

mirror :: Tree a -> Tree a
mirror  Empty         = Empty
mirror (Branch x l r) = Branch x (mirror r) (mirror l)

full :: Tree a -> Bool
full t = t `shapeEq` fullTree (height t) ()

instance Functor Tree where
    fmap _  Empty         = Empty
    fmap f (Branch x l r) = Branch (f x) (fmap f l) (fmap f r)

shapeEq :: Tree a -> Tree b -> Bool
shapeEq x y = void x == void y
-- We're not doing 'shapeEq = (==) `on` void' because that would restrict
-- shapeEq to Trees of the same type.

data Path = Left Path | Right Path | End
            deriving (Show, Eq)

allPaths :: Int -> [Path]
allPaths 0 = [End]
allPaths k = let prevPaths = allPaths (pred k) in [Left x | x <- prevPaths] ++ [Right x | x <- prevPaths]

allTrees :: Int -> a -> [Tree a]
allTrees n _ | n < 0 = error "nope"
allTrees 0 _ = [Empty]
allTrees n x = let half = [Branch x l r | m <- [0..(n-1)`div`2], l <- allTrees (n-m-1) x, r <- allTrees m x]
               in half ++ map flip [t | t <- half, not (exactlyBalanced t)]

allTreesOfMaxHeight :: Int -> a -> [Tree a]
allTreesOfMaxHeight 0 _ = [Empty]
allTreesOfMaxHeight h x = let prevTrees = allTreesOfMaxHeight (pred h) x in
                          Empty : [Branch x l r | l <- prevTrees, r <- prevTrees]

allTreesOfHeight :: Int -> a -> [Tree a]
allTreesOfHeight h x = filter ((h ==) . height) $ allTreesOfMaxHeight h x

fullTree :: Int -> a -> Tree a
fullTree l _ | l < 0 = error "nope"
fullTree 0 _ = Empty
fullTree 1 x = leaf x
fullTree l x = let oneLower = fullTree (pred l) x in Branch x oneLower oneLower

fullSize 0 = 0
fullSize l = fullSize (l-1) + 2^^(l-1)

size :: Tree a -> Int
size  Empty         = 0
size (Branch _ l r) = 1 + size l + size r

replace :: Tree a -> Tree a -> Path -> Tree a
replace  _             new  End       = new
replace  Empty         _    _         = error "path leads outside of original Tree"
replace (Branch x l r) new (Left  ps) = Branch x (replace l new ps) r
replace (Branch x l r) new (Right ps) = Branch x l (replace r new ps)

subTrees :: Tree a -> [Tree a]
subTrees    Empty         = []
subTrees t@(Branch _ l r) = t : (subTrees l ++ subTrees r)

balanced :: Tree a -> Bool
balanced  Empty         = True
balanced (Branch _ l r) = abs (size l - size r) <= 1

exactlyBalanced :: Tree a -> Bool
exactlyBalanced  Empty         = True
exactlyBalanced (Branch _ l r) = size l == size r

cbalanced :: Tree a -> Bool
cbalanced = all balanced . subTrees

-- Problem 54A
-- From the official problem statement on <wiki.haskell.org>:
-- "Haskell's type system ensures that all terms of type Tree a
-- are binary trees: it is just not possible to construct an
-- invalid tree with this type."
isTree :: Tree a -> Bool
isTree = const True

-- Problem 55
cbalTree :: Int -> a -> [Tree a]
cbalTree n _ | n < 0 = error "nope"
cbalTree n x = filter cbalanced candidates
    where top         = fullTree topLevels x
          topLevels   = let check n l = if fromIntegral n < fullSize l then (l-1) else check n (l+1) in check n 0
          bottomNodes = n - floor (fullSize topLevels)
          candidates
            | 0 == bottomNodes = [top]
            | otherwise        = map (complete top) (combinations bottomNodes $ allPaths topLevels)
          complete t []     = t
          complete t (p:ps) = complete (replace t (leaf x) p) ps

-- Use brute force (performs awful in comparison)
cbalTree' = filter cbalanced .: allTrees

-- Problem 56
symmetric :: Tree a -> Bool
symmetric t = t `shapeEq` mirror t

-- Problem 57
construct :: Ord a => [a] -> Tree a
construct = foldl insert Empty

insert :: Ord a => Tree a -> a -> Tree a
insert    Empty         x = leaf x
insert t@(Branch y l r) x
    | x < y     = Branch y (insert l x) r
    | y < x     = Branch y l (insert r x)
    | otherwise = t

-- Problem 58
-- Prolog might not make these things very easy but Haskell does
symCbalTrees :: Int -> a -> [Tree a]
symCbalTrees = filter symmetric .: cbalTree

-- Problem 59
hbalTree :: Int -> a -> [Tree a]
hbalTree = filter hbalanced .: allTreesOfHeight

hbalanced = all (\ (Branch _ l r) -> abs (height l - height r) <= 1) . subTrees

-- Problem 60
hbalTreeNodes :: Int -> a -> [Tree a]
hbalTreeNodes = filter hbalanced .: allTrees

-- Problem 61
countLeaves :: Tree a -> Int
countLeaves  Empty                 = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l     r    ) = countLeaves l + countLeaves r

-- Problem 61A
leaves :: Tree a -> [a]
leaves  Empty                 = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch x l     r    ) = leaves l ++ leaves r

-- Problem 62
internals :: Tree a -> [a]
internals  Empty                 = []
internals (Branch x Empty Empty) = []
internals (Branch x l     r)     = x : internals l ++ internals r

-- Problem 63
atLevel :: Int -> Tree a -> [a]
atLevel k  _  |  k < 0   = error "nope"
atLevel _  Empty         = []
atLevel 0 (Branch x _ _) = [x]
atLevel k (Branch _ l r) = atLevel (pred k) l ++ atLevel (pred k) r

