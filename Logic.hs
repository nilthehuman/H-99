--  !!  ATTENTION: SPOILERS AHEAD  !!  --

module Logic where

import Data.List        ( intercalate, minimumBy, partition )
import Data.Function    ( on )
import Data.Composition ( (.:) )

import Control.Arrow    ( (&&&) )
import Control.Monad    ( replicateM )

import Lists

{-# ANN module "HLint: ignore Use fmap"    #-}
{-# ANN module "HLint: ignore Use mappend" #-}

-- Problem 46
myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _    _    = False

myOr :: Bool -> Bool -> Bool
myOr False False = False
myOr _     _     = True

nand :: Bool -> Bool -> Bool
nand x y = not $ myAnd x y

nor :: Bool -> Bool -> Bool
nor  x y = not $ myOr  x y

xor :: Bool -> Bool -> Bool
xor x y = x /= y

equ :: Bool -> Bool -> Bool
equ x y = x == y

impl :: Bool -> Bool -> Bool
impl x y = myOr (not x) y

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table f = [ (x, y, f x y) | x <- [True,False], y <- [True,False] ]

printTable :: (Bool -> Bool -> Bool) -> IO ()
printTable = putStrLn . intercalate "\n" . map showTriple . table
    where showTriple (x,y,z) = show x ++ " " ++ show y ++ " " ++ show z

-- Problem 47
-- uh, any function can be infixed in Haskell so this looks like a Prolog-only problem?
-- maybe let's define some precedence levels:
infix  4 `equ`
infixr 3 `myAnd`
infixr 3 `nand`
infixr 2 `myOr`
infixr 2 `nor`
infixr 2 `xor`
infixr 1 `impl`

-- Problem 48
tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n _ | n < 0 = error "invalid argument"
tablen n f = map (\i -> i ++ [f i]) inputs
    where inputs = replicateM n [True,False]

printTablen :: Int -> ([Bool] -> Bool) -> IO ()
printTablen = putStrLn . intercalate "\n" . map showList .: tablen
    where showList = unwords . map show

-- Problem 49
gray :: Int -> [String]
gray x | x < 0 = error "invalid argument"
gray 0 = [""]
gray x = let grayPrev = gray (pred x) in map ('0':) grayPrev ++ map ('1':) (reverse grayPrev)

-- Problem 50
huffmann :: Eq a => [(a, Int)] -> [(a, String)]
huffmann xs = extract $ until done go (prepare 0 xs)
    where prepare _ []         = []
          prepare r ((a,b):xs) = (a, b, "", r) : prepare (succ r) xs

          go xs      = let (m2,r') = (min2 &&& rNext) xs in
                       map (\(a,b,c,r) -> if r == fst m2 then (a,b,'0':c,r') else
                                          if r == snd m2 then (a,b,'1':c,r') else
                                                              (a,b,    c,r )) xs
          root (_,_,_,r) = r
          rNext      = succ . maximum . map root
          minRoot    = root . minimumBy (compare `on` \(_,b,_,_) -> b)
          -- I don't like the following two much, not very elegant
          group      = consume (\acc fr@((a,_,c,r):_) -> (a, sum . map (\(_,b,_,_) -> b) $ fr, c, r) : acc)
                               (\bk@(x:_) -> partition ((root x ==) . root) bk)
                               []
          min2 xs    = let gs  = group   xs in
                       let min = minRoot gs in (min, minRoot [g | g <- gs, min /= root g])

          done []    = True
          done list  = let (r:rs) = map root list in all (r==) rs
          order      = map fst xs
          extract xs = map (\a -> lookup' a xs) order
          lookup' k ((a,_,c,_):xs) = if k == a then (a,c) else lookup' k xs

