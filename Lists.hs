--  !!  ATTENTION: SPOILERS AHEAD  !!  --

{-# LANGUAGE NoMonomorphismRestriction #-}

module Lists where

import Control.Applicative ( (<*>) )
import Control.Arrow ( (&&&) )
import Control.Monad ( replicateM )

import System.IO ( withBinaryFile, IOMode(..) )
import qualified System.Random as R

import Data.ByteString.Lazy ( hGet )
import Data.Function ( on )
import Data.List ( delete, partition, unfoldr, (\\) )

import Unsafe.Coerce  -- um...

{-# ANN module "HLint: ignore Use fmap"    #-}
{-# ANN module "HLint: ignore Use mappend" #-}

-- Problem 1
myLast :: [a] -> a
myLast []     = error "list too short"
myLast [x]    = x
myLast (_:xs) = myLast xs

myLast' = foldl1 (curry snd)

-- Problem 2
myButLast :: [a] -> a
myButLast []     = error "list too short"
myButLast [_]    = error "list too short"
myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

myButLast' = fst . foldl shift (undefined, undefined)
    where shift (_,y) z = (y,z)

-- pointfree's version
myButLast'' = fst . foldl ((,) . snd) (undefined, undefined)

-- Problem 3
elementAt :: Integral i => [a] -> i -> a
elementAt (x:_)  1 = x
elementAt (_:xs) i = elementAt xs (i-1)
elementAt _      _ = error "out of bounds"

elementAt' list i = fst . foldl pick (undefined, i) $ list
    where pick (z, i) x
            | i == 1    = (x, 0)
            | otherwise = (z, i-1)

elementAt'' list i = snd . head . filter ((i==) . fst) $ zip [1,2..] list

-- Problem 4
myLength :: [a] -> Int
myLength = foldr ((succ .) . curry snd) 0

myLength' = sum . map (const 1)

myLength'' = fst . last . zip [0,1..] . (undefined:)

myLength''' = last . zipWith (curry fst) [0,1..] . (undefined:)

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myReverse' = helper []
    where helper acc []     = acc  -- hlint says "use foldl"; no shit
          helper acc (x:xs) = helper (x:acc) xs

myReverse'' []     = []
myReverse'' (x:xs) = myReverse'' xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = uncurry (==) . (id &&& reverse)

isPalindrome' l = let halflen = fromIntegral (length l) * 0.5 in
                  let (front, back) = (take (floor halflen) l, drop (ceiling halflen) l) in
                  front == reverse back

isPalindrome'' l = all check [0..length l `div` 2 - 1]
    where check i = l !! i == l !! (length l - i - 1)

-- Problem 7
data NestedList a = Elem a | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List l)  = foldl cat [] l  -- it's better to use concatMap here
    where cat acc nl = acc ++ flatten nl

-- Problem 8
compress :: Eq a => [a] -> [a]
compress []   = []
compress list = foldr step [last list] (init list)
    where step x acc
            | x == head acc =   acc
            | otherwise     = x:acc

-- Let's extract a useful combinator that will help us solve Problems 9 and 10 too
-- (generalizable to all Foldables but lists will do fine here)
consume :: (z -> [a] -> z) -> ([a] -> ([a], [a])) -> z -> [a] -> z
consume _ _ acc []   = acc
consume f g acc list = let (front, back) = g list in
                       consume f g (f acc front) back

-- a cleaner implementation by Gesh using standard combinators
consume' :: (z -> b -> z) -> ([a] -> (b, [a])) -> z -> [a] -> z
consume' f g a = foldl f a . chunk g
chunk g = unfoldr (fmap g . (\xs -> if null xs then Nothing else Just xs))

-- a specialization of the above
consumeEqual gather = consume gather split []
    where split (x:xs) = span (== x) (x:xs)

compress' = consumeEqual (\acc front -> acc ++ [head front])

-- this should perform better than compress'
compress'' = reverse . consumeEqual (\acc front -> head front : acc)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack []   = []
pack list = foldr step [[last list]] (init list)
    where step x acc
            | x == (head . head $ acc) = (x:head acc):tail acc
            | otherwise                = [x]:acc

pack' = consumeEqual (\acc front -> acc ++ [front])

-- this should perform better than pack'
pack'' = reverse . consumeEqual (\acc front -> front : acc)

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = consumeEqual (\acc front -> acc ++ [(length front, head front)])

-- At this point it seems to me that the Prelude's standard combinators are
-- so complete that using explicit recursion on lists can be considered a code smell.

-- Problem 11
data ListElem a = Single a | Multiple Int a
    deriving Show

encodeModified :: Eq a => [a] -> [ListElem a]
encodeModified = consumeEqual (\acc front -> acc ++ munge front)
    where munge [x]    = [Single                   x]
          munge (x:xs) = [Multiple (length (x:xs)) x]
          munge _      = error "this should never happen"

-- this should perform better than encodeModified
encodeModified' = reverse . consumeEqual (\acc front -> munge front : acc)
    where munge [x]    = Single                   x
          munge (x:xs) = Multiple (length (x:xs)) x
          munge _      = error "this should never happen"

-- Problem 12
decodeModified :: [ListElem a] -> [a]
decodeModified = concatMap demunge
    where demunge (Single     x) = [x]
          demunge (Multiple i x) = replicate i x

-- Problem 13
encodeDirect :: Eq a => [a] -> [ListElem a]
encodeDirect []     = []
encodeDirect (x:xs) = reverse . foldl step [Single x] $ xs
    -- this feels clumsy compared to encodeModified
    where step (Single     x : xs) y = if x == y then Multiple 2        x : xs else Single y : Single     x : xs
          step (Multiple i x : xs) y = if x == y then Multiple (succ i) x : xs else Single y : Multiple i x : xs

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli' = concatMap (replicate 2)

dupli'' = foldr (\x acc -> x:x:acc) []

-- Problem 15
repli :: [a] -> Int -> [a]
repli []     _ = []
repli (x:xs) k = r x k ++ repli xs k
    where r _ 0 = []
          r x i
            | i < 0     = error "negative argument"  -- don't let k < 0 bottom out
            | otherwise = x:r x (pred i)

repli' = flip (concatMap . replicate)

repli'' xs k = foldr (\x acc -> replicate k x ++ acc) [] xs

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs k = go xs k
    where go []     _ = []
          go (_:xs) 1 = go xs k
          go (x:xs) i
            | i < 1     = error "invalid argument"  -- don't let k < 0 bottom out
            | otherwise = x:go xs (pred i)

dropEvery' xs k = map snd . filter ((0/=) . (flip mod) k . fst) $ zip [1..] xs

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split []     _ = ([], [])
split xs     0 = ([], xs)
split (x:xs) n
    | n < 0     = error "invalid argument"
    | otherwise = let (front, back) = split xs (pred n) in (x:front, back)

split' xs n = let (front, back) = go xs n [] in (reverse front, back)  -- I hope using reverse doesn't count as cheating
    where go [] _ acc = (acc, [])
          go xs 0 acc = (acc, xs)
          go (x:xs) i acc
              | i < 0     = error "invalid argument"
              | otherwise = go xs (pred i) (x:acc)

split'' xs n
    | n < 0     = error "invalid argument"
    | otherwise = both (map fst) . spt $ zip xs [0..]
    where both f (x, y) = (f x, f y)
          spt []     = ([], [])
          spt (x:xs) = if n == snd x then ([], x:xs) else let (front, back) = spt xs in (x:front, back)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice []     _ _ = []
slice (x:xs) i k
    | k < i     = error "invalid arguments"
    | k < 1     = []
    | otherwise = if 1 < i || k < 1 then rest else x:rest
        where rest = slice xs (pred i) (pred k)

slice' xs i k = map fst . filter (\x -> i <= snd x && snd x <= k) $ zip xs [1..]

slice'' xs i k = drop (pred i) . take k $ xs

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n = let n' = if n < 0 then (length xs + n) else n in drop n' xs ++ take n' xs

-- Problem 20
removeAt :: [a] -> Int -> (a, [a])
removeAt []     _ = error "empty list"
removeAt (x:xs) 1 = (x, xs)
removeAt (x:xs) k
    | k < 1     = error "invalid argument"
    | otherwise = let (y, ys) = removeAt xs (pred k) in (y, x:ys)

removeAt' xs k = let (front, back) = split xs (pred k) in (head back, front ++ tail back)

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs     1 = x:xs
insertAt x (y:ys) k
    | k < 1                  = error "invalid argument"
    | length (y:ys) < pred k = error "invalid argument"
    | otherwise              = y : insertAt x ys (pred k)

insertAt' x xs k = take (pred k) xs ++ x : drop (pred k) xs

-- Problem 22
range :: Int -> Int -> [Int]
range m n
    | n < m     = error "invalid arguments"
    | m == n    = [m]
    | otherwise = m:range (succ m) n

range' m n = slice [1..] m n  -- won't work for non-positive indices though

range'' m n = take (n-m+1) . drop m $ [0..]  -- won't work for negative indices

-- Problem 23
-- now things get a little messy
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = withBinaryFile source ReadMode $ \random -> replicateM n $ do
    bs <- hGet random . floor . logBase 2 $ fromIntegral (maxBound :: Int)
    let i = unsafeCoerce bs :: Int  -- what the hell am I doing?
    return $ xs !! (i `mod` length xs)
    where source = "/dev/random"

rnd_select' :: [a] -> Int -> [a]  -- the type tells you this function is actually deterministic
rnd_select' xs n = take n . map (xs !!) $ rIndices
    where rs g = let (i, g') = R.randomR (0, pred . length $ xs) g in i : rs g'
          rIndices = rs $ R.mkStdGen n  -- use whatever little entropy we're given in n

-- Problem 24
generic_diff_select :: Eq a => [a] -> Int -> [a]
generic_diff_select xs n
    | length xs < n = error "invalid arguments"
    | n < 0         = error "invalid arguments"
    | otherwise     = take n . pick xs $ rIndices
    where pick xs (i:is) = let x = xs !! i in x : pick (delete x xs) is
          rs g k = let (i, g') = R.randomR (0, k) g in i : rs g' (pred k)
          rIndices = rs (R.mkStdGen n) (pred . length $ xs)

diff_select :: Int -> Int -> [Int]
diff_select n m = generic_diff_select [1..m] n

-- Problem 25
rnd_permu :: Eq a => [a] -> [a]
rnd_permu = generic_diff_select <*> length

-- Problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k (x:xs) = [ x:cs | cs <- combinations (k-1) xs ] ++ combinations k xs

-- Problem 27
group :: Eq a => [Int] -> [a] -> [[[a]]]
group ks xs
    | sum ks /= length xs = error "invalid arguments"
    | otherwise           = foldl (flip setApart) [[xs]] . reverse . tail $ ks
    where setApart k xs     = concatMap (go k) xs
          go       k (g:gs) = [ (g \\ c) : c : gs | c <- combinations k g ]

-- Problem 28
-- let's first implement quicksort and go from there
qsort :: (a -> a -> Ordering) -> [a] -> [a]
qsort _   []     = []
qsort cmp (x:xs) = qsort cmp lower ++ [x] ++ qsort cmp rest
    where (lower, rest) = partition (\y -> LT == cmp y x) xs

-- a)
lsort :: [[a]] -> [[a]]
lsort = qsort (compare `on` length)
        -- this is the same as: qsort (\x y -> compare (length x) (length y))

-- b)
lfsort :: [[a]] -> [[a]]
lfsort xs = map fst . qsort (compare `on` snd) $ map (fmap count) xsWithLengths
    where xsWithLengths = zipWith ((. length) . (,)) xs xs
          count i       = length . filter ((i==) . snd) $ xsWithLengths
