module Polynomial where

import qualified Prelude as P
import Prelude(Int,Show,foldr,zip,length,map,zip3,splitAt,snd,(.),tail,(++),($),(!!),take)
import Field

newtype Polynomial a = Polynomial [a] deriving (Show)

constant :: Polynomial t -> [t]
constant (Polynomial i) =  i

evaluate :: Field a => Polynomial a -> a -> a
evaluate f x = foldr (+) zero [ r*xn | (r,xn) <- zip (constant f) xpow] where
        xpow = map (power x) [0..length (constant f) P.- 1]

-- The first co-efficient in a polynomial
s0 :: Field a => [a] -> [a] -> a
s0 x y = power (zero - one) (length x P.+ 1) * foldr (*) one x * sumyxc
    where
      sumyxc  = foldr (+) zero [yi/(xi*ci) | (xi,yi,ci) <- zip3 x y c]
      c       = map (ceye x) [0..length x P.- 1]

-- Computes the ith component of the vector c in lagrange basis expansion
ceye :: Field a => [a] -> Int -> a
ceye x i = foldr (\y z -> ((x!!i)-y)*z) one $ take i x ++ (tail . snd) (splitAt i x)
