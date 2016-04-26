module Main where

import Test.QuickCheck

import Tests

testAll :: IO ()
testAll = mapM_ quickCheck tests

main :: IO ()
main = testAll
