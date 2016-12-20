--  !!  ATTENTION: SPOILERS AHEAD  !!  --

module MultiwayTrees where

import Data.List  ( foldl' )
import Data.Maybe ( isJust, isNothing )
import Data.Tree

{-# ANN module "HLint: ignore Use fmap" #-}

-- Problem 70C
count :: Integral b => Tree a -> b
count (Node _ ts) = succ . sum . map count $ ts

-- Problem 70
stringToTree :: String -> Tree Char
stringToTree = strip . foldl' build empty
    where empty = Node Nothing []
          hasEmpty (Node _ ts) = any (isNothing . rootLabel) ts
          build :: Tree (Maybe Char) -> Char -> Tree (Maybe Char)
          build (Node Nothing []) '^' = error "malformed input string (unexpected '^')"
          build (Node Nothing []) c   = Node (Just c) [empty]
          build (Node x       ts) '^' = if any hasEmpty ts
                                        then Node x (empty : map (flip build $ '^') ts)
                                        else Node x (map (flip build $ '^') . filter (isJust . rootLabel) $ ts)
          build (Node x       ts) c   = Node x (map (flip build $ c) ts)
          strip :: Tree (Maybe Char) -> Tree Char
          strip (Node Nothing  _ ) = error "empty input string"
          strip (Node (Just c) ts) = Node c (map strip . filter (isJust . rootLabel) $ ts)

-- Problem 71
ipl :: Integral b => Tree a -> b
ipl   (Node _ []) = 0
ipl t@(Node x ts) = go 1 t
    where go d = sum . map ((+d) . go (d+1)) . subForest

