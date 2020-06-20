module Polynomial where

import qualified Prelude as P
import Prelude(Int,Show,foldr,zip,length,map,zip3,splitAt,snd,(.),tail,(++),($),(!!),take)
import Field as F
import qualified Data.Matrix as Mat
import qualified Math as M
import Debug.Trace

newtype Polynomial a = Polynomial [a] deriving (Show)


-- Taken from "Beginner's guide to mapping simplexes affinely" Vitalii Bohdanovych Tymchyshyn
-- takes a number from 1-> n and two lists of size n+1 and gives back the mth coefficient
-- The first co-efficient in a polynomial
coeff :: (Field a,Show a) => Int -> [a] -> [a] -> a
coeff m x y = sign F.* m1 F./ (M.myDet bottomMat) where
--the problem is with topMatrix
  -- join x to the matrix and then remove the first column so I can use the function minor Matrix
        m1         = M.myDet $ Mat.minorMatrix (n1 P.-m) 1 (Mat.fromList (n1 P.+1) 1  (x P.++ [F.zero]) Mat.<|> topMatrix)
        -- m2 is the matrix on the bottom of the divide sign in the paper referenced above
        bottomMat  = Mat.matrix n1 n1 (\(i,j) -> F.power (x P.!!(j P.-1)) (n1 P.- i))
        --n+1 or the length of x and y
        n1         = length x
        topMatrix  = Mat.fromList 1 n1 y Mat.<-> bottomMat
        -- multiplicative sign depending on which row it is n
        -- if  (n1+1-m) is odd then one else -one
        sign       = if P.mod (n1 P.+ 1 P.-m) 2 P.== 0 then (F.zero F.- F.one) else F.one



--constant :: Polynomial t -> [t]
constant (Polynomial i) =  i

-- evaluates a Polynomial at a given point
evaluate :: Field a => Polynomial a -> a -> a
evaluate f x = foldr (+) zero [ r*xn | (r, xn) <- zip (constant f) xpow ] where
        xpow = map (power x) [0..length (constant f) P.- 1]


-- Computes the ith component of the vector c in lagrange basis expansion
ceye :: Field a => [a] -> Int -> a
ceye x i = foldr (\y z -> ((x!!i)-y)*z) one $ take i x ++ (tail . snd) (splitAt i x)
