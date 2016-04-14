--  !!  ATTENTION: SPOILERS AHEAD  !!  --

{-# LANGUAGE NoMonomorphismRestriction #-}

module Arithmetic where

import Control.Applicative ( liftA2 )
import Control.Arrow       ( (&&&) )

import Lists ( consumeEqual )

{-# ANN module "HLint: ignore Use fmap" #-}

divides :: Integral a => a -> a -> Bool
divides c x = x `mod` c == 0

-- Problem 31
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime 2 = True  -- this allows us to make `candidates' below general
isPrime x = not . any (`divides` abs x) $ candidates
        where
            candidates    = 2:[3,5..maxCandidate]
            maxCandidate  = floor . sqrt . fromIntegral $ abs x

-- sieve algorithm; this version is way slower though and has terrible space cost
isPrime' x
    | x < 0     = isPrime' (-x)
    | x < 2     = False
    | otherwise = x == last primes
    where primes = until (((maxCandidate<) . head) `higherOr` ((x /=) . last))
                         (\(p:ps) -> filter (not . divides p) ps)
                         [2..x]
          higherOr = liftA2 (||)
          maxCandidate = floor . sqrt . fromIntegral $ x

-- Problem 32
myGCD :: Integral a => a -> a -> a
myGCD x y
    | ax < ay   = helper ax ay
    | otherwise = helper ay ax
    where (ax, ay) = (abs x, abs y)
          helper x 0 = x
          helper 0 y = y
          helper x y = helper (y `mod` x) x
          -- this can also be expressed as:
          -- helper x y = snd $ until ((0==) . fst) (\(a,b) -> (b `mod` a, a)) (x, y)

-- Problem 33
coprime :: Integral a => a -> a -> Bool
coprime = ((.).(.)) (1 ==) myGCD  -- the "startled owl" combinator

-- this one's a tad slower
coprime' x y = not $ any ((`divides` x) `higherAnd` (`divides` y)) candidates
    where
          higherAnd    = liftA2 (&&)
          candidates   = 2:[3,5..maxCandidate]
          maxCandidate = floor . sqrt . fromIntegral $ min x y

-- Problem 34
totient :: Integral a => a -> Int
totient   x = length . filter (coprime x) $ [1..x-1]

totient'  x = foldl (\acc y -> if coprime x y then acc+1 else acc) 0 [1..x-1]

totient'' x = sum . map (\y -> if coprime x y then     1 else   0) $ [1..x-1]

-- Problem 35
primeFactors :: Integral a => a -> [a]
primeFactors 0 = error "invalid argument"
primeFactors x = go x 2
    where go 1 _ = []
          go x p
           | p `divides` x = p : go (x `div` p) p
           | otherwise     =     go  x         (p+1)

primeFactors' 0 = error "invalid argument"
primeFactors' x = go x 2 []
    where go 1 _ acc = reverse acc
          go x p acc
           | p `divides` x = go (x `div` p) p    (p:acc)
           | otherwise     = go  x         (p+1)    acc

-- this one turns out to be slower
primeFactors'' 0 = error "invalid argument"
primeFactors'' x = let factors = go x candidates [] in if null factors then [x] else factors
    where go 1 _         acc = reverse acc
          go _ []        acc = reverse acc
          go x cs@(p:ps) acc
           | p `divides` x = go (x `div` p)                               cs  (p:acc)
           | otherwise     = go  x          (filter (not . (p `divides`)) ps)    acc
          candidates   = 2:[3,5..maxCandidate]
          maxCandidate = floor . (/2) . fromIntegral $ x

-- Problem 36
primeFactorsMult :: Integral a => a -> [(a, Int)]
primeFactorsMult = reverse . consumeEqual (\acc x -> (head &&& length) x : acc) . primeFactors

primeFactorsMult' 0 = error "invalid argument"
primeFactorsMult' x = go x 2 []
    where go 1 _ acc = reverse acc
          go x p acc
           | p `divides` x = go (x `div` p) p    (bump p acc)
           | otherwise     = go  x         (p+1)         acc
          bump p []     = [(p,1)]
          bump p (a:as) = if p == fst a then fmap (+1) a : as
                                        else (p,1)   : a : as

-- Problem 37
totientImproved :: Int -> Int
totientImproved = product . map (floor . formula) . primeFactorsMult
    where formula (p, m) = let p' = fromIntegral p in (p'-1) * p' ^^ (m-1)

-- Problem 38
-- totient 10 == length . filter (coprime 10) $ [1..9]
--               ...snip ~30 reductions...
--            == length [1,3,7,9]
--               ...snip 4 more reductions...
--            == 4

-- totientImproved 10 == product . map (floor . formula) . primeFactorsMult' 10
--                       ...snip ~10 reductions...
--                    == product . map (floor . formula) $ [2,5]
--                       ...snip ~8 more reductions...
--                    == 4

-- totientImproved turns out to perform many orders of magnitude better

