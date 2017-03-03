module Tests where

import Data.Ratio
import Data.List           ( maximumBy, minimumBy, nub, nubBy )
import Data.Function       ( on )
import Data.Composition    ( (.:) )

import Control.Applicative ( liftA2 )
import Control.Arrow       ( first, (&&&) )

import Arithmetic
import BinaryTrees hiding  ( flip )
import Lists
import Logic

tests1to10 :: [[Int] -> Int -> Bool]
tests1to10 = map (\f l x -> f (l ++ [x]          ) == x)                   [   myLast,    myLast'] ++
             map (\f l x -> f (l ++ [x,undefined]) == x)                   [myButLast, myButLast', myButLast''] ++
             map (\f l i -> i < 1 || length l < i  || f l i == l !! (i-1)) [elementAt, elementAt', elementAt''] ++
             map (\f l _ -> f l == length  l)                              [ myLength,  myLength',  myLength'',  myLength'''] ++
             map (\f l _ -> f l == reverse l)                              [myReverse, myReverse', myReverse''] ++
             map (\f l i -> i < 0 || length l <= i || not (f l) || l !! i == l !! (length l - i - 1))             [isPalindrome, isPalindrome', isPalindrome''] ++
             -- no test for flatten
             map (\f l i -> let res = f l in (i < 1 || length res <= i || (res !! (i-1) /= res !! i)))            [compress, compress', compress''] ++
             map (\f l i -> let res = f l in (i < 0 || length res <= i || let p = res !! i in all (== head p) p)) [pack, pack', pack''] ++
             map (\f l _ -> length l == sum (map fst $ f l))                                                      [encode]

tests11to20 :: [[Int] -> Int -> Bool]
tests11to20 = map (\f l _ -> length l == sum (map int $ f l))                         [encodeModified, encodeModified'] ++
              map (\f l i -> let l' = map (if i < 1 then Single else Multiple i) l in
                                      length (f l') == sum (map int l'))              [decodeModified] ++
              map (\f l _ -> length l == sum (map int $ f l))                         [encodeDirect] ++
              map (\f l i -> i < 0 || length l <= i || l !! i == (f l)   !! (i*2))    [dupli, dupli', dupli''] ++
              map (\f l i -> i < 1 || length l <= i || l !! i == (f l i) !! (i*i))    [repli, repli', repli''] ++
              map (\f l i -> i < 1 || let res = f l i in length res == ceiling ((1 - 1%i) * (length l%1)))  [dropEvery, dropEvery'] ++
              map (\f l i -> i < 0 || let res = f l i in length (fst res) == min i        (length l))       [split, split', split''] ++
              map (\f l i -> i < 1 || let res = f l i (i*2) in length res <= min (succ i) (length l))       [slice, slice', slice''] ++
              map (\f l i -> length l <= (succ $ abs i) || let res = f l i in head res == l !! if i < 0 then length l + i else i) [rotate] ++
              map (\f l i -> i < 1 || length l < i || let res = f l i in fst res == l !! (pred i))          [removeAt, removeAt']
              where int (Single     _) = 1
                    int (Multiple i _) = i

tests21to28 :: [[Int] -> Int -> Bool]
tests21to28 = map (\f l i -> i < 1 || length l < i || let res = f (head l) l i in res !! pred i == head l)                         [insertAt, insertAt'] ++
              map (\f l i -> i < 1 || length l < 1 || head l < i || let res = f i (head l) in head res == i && last res == head l) [range, range', range''] ++
              -- no tests for rnd_select, diff_select or rnd_permu
              map (\f l i -> i < 0 || let len = length $ nub l in len < i || fromIntegral(length $ f i $ nub l) == binomial len i) [combinations] ++
              map (\f l i -> i < 0 || let len = length $ nub l in len < i || fromIntegral(length $ f [i,len-i] $ nub l) == binomial len i) [group] ++
              map (\f l _ -> let l' = map (flip replicate $ 'x') l in let res = f l' in qsort compare l' == qsort compare res)     [lsort, lfsort]

tests31to41 :: [[Int] -> Int -> Bool]
tests31to41 = map (\f _ _ -> f 2377  == True)                                                                    [isPrime, isPrime'] ++
              map (\f l x -> null l || let y = (head l) in 0 == x || 0 == y || f x y <= min (abs x) (abs y))     [myGCD] ++
              map (\f _ _ -> f 35 64 == True)                                                                    [coprime, coprime'] ++
              map (\f _ x -> x < 2  || f x < x)                                                                  [totient, totient', totient'', totientImproved] ++
              map (\f _ x -> x < 1  || (product . f $ x) == x)                                                   [primeFactors, primeFactors', primeFactors''] ++
              map (\f _ x -> x < 1  || (product . map (floor . uncurry (^^) . first fromIntegral) . f $ x) == x) [primeFactorsMult, primeFactorsMult'] ++
              map (\f l x -> null l || let y = head l in all isPrime (f x y))                                    [primesR, primesR'] ++
              map (\f _ x -> x < 4  || odd x || let (p,r) = f x in isPrime p && isPrime r && p + r == x)         [goldbach] ++
              map (\f l x -> null l || let y = head l in x < 4 || y < 4 || odd x || odd y ||
                                                               (all (\(p,r) -> isPrime p && isPrime r) $ f x y)) [goldbachList, \x y -> goldbachList' x y 50]

tests46to50 :: [[Int] -> Int -> Bool]
tests46to50 = map (\f _ _ ->          all (\(x,y,z) ->        f x y       == z      ) (table            f) )     [myAnd, myOr, nand, nor, xor, equ, impl] ++
              map (\f _ i -> i < 1 || all (\ xs     -> foldl1 f (init xs) == last xs) (tablen i (foldl1 f)))     [myAnd, myOr, nand, nor, xor, equ, impl] ++
              map (\f _ i -> i < 0 || 10 < i || (liftA2 (==) (floor . (2^^)) (length . f) i))                    [gray] ++
              map (\f l _ -> null l || let l' = f (zip [0..] l) in all ((floor(2^^(length l - 1)) >) . fromIntegral . length . snd) l') [huffmann]

tests54to60 :: [[Int] -> Int -> Bool]
tests54to60 = map (\f _ i -> f (allTrees 10 () !! i) == True)                [isTree] ++
              map (\f _ i -> i < 0 || 10 < i || all cbalanced (f i ()))      [cbalTree, cbalTree'] ++
              map (\f _ i -> i < 0 || 12 < i || f (fullTree i ()))           [symmetric] ++
              map (\f l _ -> length (nub l) == size (f l))                   [construct] ++
              map (\f _ i -> i < 0 || 10 < i || all (liftA2 (&&) cbalanced symmetric) (f i ())) [symCbalTrees] ++
              map (\f _ i -> i < 0 ||  4 < i || all hbalanced (f i ()))      [hbalTree, hbalTreeNodes]

tests61to69 :: [[Int] -> Int -> Bool]
tests61to69 = map (\f l _ ->          let t = construct l in f   t <= size t)       [countLeaves, length . leaves, length . internals] ++
              map (\f l i -> i < 0 || let t = construct l in f i t <= floor (2^^i)) [length .: atLevel]
              -- more tests to come

-- helper functions for the above predicates
fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact (pred n)

binomial :: Integral a => a -> a -> a
binomial n k = div (fact n) (fact k * fact (n-k))

tests = concat [tests1to10, tests11to20, tests21to28, tests31to41, tests46to50, tests54to60, tests61to69]
