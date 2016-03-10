{-# LANGUAGE NoMonomorphismRestriction #-}

module Lists where

-- Problem 1
myLast :: [a] -> a
myLast []     = error "list too short"
myLast [x]    = x
myLast (_:xs) = myLast xs

myLast' = foldl (curry snd) undefined

-- Problem 2
myButLast :: [a] -> a
myButLast []     = error "list too short"
myButLast [_]    = error "list too short"
myButLast [x,_]  = x
myButLast (_:xs) = myButLast xs

myButLast' = fst . foldl shift (undefined, undefined)
    where shift (x,y) z = (y,z)

-- pointfree's version
myButLast'' = fst . foldl ((,) . snd) (undefined, undefined)

-- Problem 3
elementAt :: Integral i => [a] -> i -> a
elementAt (x:_)  1 = x
elementAt (_:xs) i = elementAt xs (i-1)
elementAt _      _ = error "out of bounds"

elementAt' list i = fst $ foldl pick (undefined, i) list
    where pick (z, i) x
            | i == 1    = (x, 0)
            | otherwise = (z, i-1)

-- pointfree's suggestion, wow
elementAt'' = (fst .) . flip (foldl pick . (,) undefined)
    where pick (z, i) x
            | i == 1    = (x, 0)
            | otherwise = (z, i-1)

elementAt''' list i = snd . head . filter ((i==) . fst) $ zip [1,2..] list

-- Problem 4
myLength :: [a] -> Int
myLength = foldr ((succ .) . curry snd) 0

myLength' = sum . map (const 1)

myLength'' []   = 0
myLength'' list = fst . last $ zip [1,2..] list

myLength''' []   = 0
myLength''' list = last $ zipWith (curry fst) [1,2..] list

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
isPalindrome l = l == reverse l

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

