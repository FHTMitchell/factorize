module Factors where 

import System.Environment
import Data.List

-- integer methods

-- |Integer square root (floor)
isqrt :: (Integral i) => i -> i
isqrt n = floor $ sqrt $ fromIntegral n

-- |True if y is a factor of x
divides :: (Integral i) => i -> i -> Bool
divides x y = (x `mod` y) == 0

-- list helpers

-- |Just the first value for which f is True, Nothing if no such value exists                              
firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat f xs = case filter f xs of
                      [] -> Nothing
                      (x:_) -> Just x                      
                      
-- prime numbers

-- |Every prime number
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]  -- this is memoized 

-- |Check if a number is prime
isPrime :: Integer -> Bool
isPrime n | n >= 2 = all (not . divides n) (takeWhile (<= isqrt n) primes)
          | otherwise = False


-- factorization
          
-- |The prime factors of n                                               
factorize :: Integer -> [Integer]
factorize n | n <= 0 = error "n must be positive"
            | n == 1 = []
            | otherwise = factorize2 n 2
  -- use factorize 2 to drop already checked prime factors
  where factorize2 m start =
          let psDrop = dropWhile (< start) primes
              psTake = takeWhile (<= isqrt m) psDrop
              in case firstThat (divides m) psTake of
                      Nothing -> [m]
                      Just fact -> fact : factorize2 (m `quot` fact) fact
            
                                      
-- |A representation of the factors of n
showFactors :: Integer -> String
showFactors n = let fs = factorize n
                    mulShow = intercalate "*" $ map show fs
                    in (show n) ++ " == " ++ mulShow
                    
                    
                    
                    