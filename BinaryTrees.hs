--  !!  ATTENTION: SPOILERS AHEAD  !!  --

module BinaryTrees where

import Prelude hiding ( flip, Left, Right )
import Data.Function  ( on )
import Data.List      ( nubBy )
import Control.Monad  ( void )

import Lists          ( combinations )

{-# ANN module "HLint: ignore Use fmap"    #-}
{-# ANN module "HLint: ignore Use mappend" #-}
{-# ANN module "HLint: ignore Use =<<"     #-}

data Tree a = Empty | Branch { label :: a, left :: Tree a, right :: Tree a }
              deriving (Show, Eq)

-- Utilities:
leaf :: a -> Tree a
leaf x = Branch x Empty Empty

depth :: Tree a -> Int
depth  Empty         = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

flip :: Tree a -> Tree a
flip  Empty         = Empty
flip (Branch x l r) = Branch x r l

mirror :: Tree a -> Tree a
mirror  Empty         = Empty
mirror (Branch x l r) = Branch x (mirror r) (mirror l)

twin :: Tree a -> Bool
twin t = let t' = void t in t' == flip t'

symm :: Tree a -> Bool
symm t = let t' = void t in t' == mirror t'

full :: Tree a -> Bool
full t = t `shapeEq` fullTree (depth t) ()

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
allTrees 1 x = [leaf x]
allTrees n x = let half = [Branch x l r | m <- [0..(n-1)`div`2], l <- allTrees (n-m-1) x, r <- allTrees m x]
               in half ++ map flip [t | t <- half, not (exactlyBalanced t)]

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
cbalTree :: Eq a => Int -> a -> [Tree a]
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
cbalTree' n x = filter cbalanced $ allTrees n x

