--  !!  ATTENTION: SPOILERS AHEAD  !!  --

module Logic where

import Data.List        ( intercalate, minimumBy, partition, sortBy )
import Data.Ord         ( comparing )
import Data.Composition ( (.:) )

import Control.Arrow    ( (&&&) )
import Control.Monad    ( replicateM )

import Lists            ( chunk, consume )

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
nand = not .: myAnd

nor :: Bool -> Bool -> Bool
nor  = not .: myOr

xor :: Bool -> Bool -> Bool
xor  = (/=)

equ :: Bool -> Bool -> Bool
equ  = (==)

impl :: Bool -> Bool -> Bool
impl = myOr . not

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
huffmann xs = extract xs . until done go $ prepare 0 xs
    where prepare _ []         = []
          prepare r ((a,b):xs) = (a,b,"",r) : prepare (succ r) xs

          go xs = map (update '0') m1 ++ map (update '1') m2 ++ concat ms
              where (m1:m2:ms) = min2 xs
                    update p (c,w,ps,_) = (c,w,p:ps,r')
                    r' = succ . maximum . map root $ xs
          root (_,_,_,r) = r
          gatherRoots = chunk $ \ l@(x:_) -> partition (\y -> root x == root y) l
          min2 = map fst . sortOn snd . map (id &&& flatten) . gatherRoots
              where flatten = sum . map weight
                    weight (_,w,_,_) = w
                    sortOn = sortBy . comparing

          done = (<=1) . length . gatherRoots
          extract xs zs = map (\a -> lookup' a zs) order
              where order = map fst xs
                    lookup' k ((a,_,c,_):zs) = if k == a then (a,c) else lookup' k zs

