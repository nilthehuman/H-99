module Main where

import Data.Ratio
import Test.QuickCheck

import Lists

-- Tests for Problems 1-10
tests1to10 :: [[Int] -> Int -> Bool]
tests1to10 = map (\f l x -> f (l ++ [x]          ) == x)                   [   myLast,    myLast'] ++
             map (\f l x -> f (l ++ [x,undefined]) == x)                   [myButLast, myButLast', myButLast''] ++
             map (\f l i -> i < 1 || length l < i  || f l i == l !! (i-1)) [elementAt, elementAt', elementAt'', elementAt'''] ++
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
              map (\f l i -> i < 1 || let res = f l i in length res == ceiling ((1 - 1%i) * (length l%1)))  [dropEvery, dropEvery']
              where int (Single     _) = 1
                    int (Multiple i _) = i

tests = concat [tests1to10, tests11to20]

testAll :: IO ()
testAll = mapM_ quickCheck tests

main :: IO ()
main = testAll
