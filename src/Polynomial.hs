module Polynomial where

import qualified Prelude as P
import Field as F
import qualified Data.Matrix as Mat
import qualified Math as M

newtype Polynomial a = Polynomial [a] deriving (P.Show)

-- Taken from "Beginner's guide to mapping simplexes affinely" Vitalii Bohdanovych Tymchyshyn
-- takes a number from 1-> n and two lists of size n+1 and gives back the mth coefficient
-- The first co-efficient in a polynomial
coeff :: Field a => P.Int -> [a] -> [a] -> a
coeff m x y = sign F.* m1 F./ M.myDet bottomMat where
--the problem is with topMatrix
  -- join x to the matrix and then remove the first column so I can use the function minor Matrix
        m1         = M.myDet P.$ Mat.minorMatrix (n1 P.+ 2 P.-m) 1 (Mat.fromList (n1 P.+1) 1  (x P.++ [F.zero]) Mat.<|> topMat)
        -- m2 is the matrix on the bottom of the divide sign in the paper referenced above
        bottomMat  = Mat.matrix n1 n1 (\(i,j) -> F.power (x P.!!(j P.-1)) (n1 P.- i))
        --n+1 or the length of x and y
        n1         = P.length x
        topMat     = Mat.fromList 1 n1 y Mat.<-> bottomMat
        -- multiplicative sign depending on which row it is n
        -- if  (n1+1-m) is odd then one else -one
        sign       = if P.mod (n1 P.-m) 2 P.== 0 then F.one else F.zero F.- F.one

constant :: Polynomial a -> [a]
constant (Polynomial i) =  i

-- evaluates a Polynomial at a given point
evaluate :: Field a => Polynomial a -> a -> a
evaluate f x = P.foldr (+) zero [ r*xn | (r, xn) <- P.zip (constant f) xpow ] where
        xpow = P.map (power x) [0..P.length (constant f) P.- 1]
