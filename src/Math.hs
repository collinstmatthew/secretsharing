-- constructing a field, end goal is to do LU factorisation and then compute the determinant
module Math where

import Prelude(($), (!!), (++), (.), Int, Show, iterate, mod, quotRem, snd)
import Data.Eq
import Data.Proxy (Proxy(..))

import Data.Matrix
import Data.Maybe(fromJust)
import Data.List(findIndex)
import Prelude
import Prelude as P
-- should actually probably creat a show instance for this
-- nieve implementation as a list of lists

-- have to implement custome LU decomposition function as the library one requires the elements
-- of a have to be ordered, I didn't look into the source code but I suspect this is to ensure
-- that when the matrix is pivoted on the largeste element is chosen to ensure stability, as we are -- as we are carrying out these computations in finite fields we do not have to worry about that

--luDecomp :: Field a => Matrix a -> (Matrix a,Matrix a)
--the Num is neccesry for the 1
--http://ressources.unisciel.fr/algoprog/s00aaroot/aa00module1/res/%5BCormen-AL2011%5DIntroduction_To_Algorithms-A3.pdf page 820
-- return type of the function will be (L,U,P)
lumDecomp :: (Fractional a, Num a, Eq a) =>  Matrix a -> (Matrix a,Matrix a)
lumDecomp m
    |  nrows m == 1       = (m, fromList 1 1 [1])
    |  getElem 1 1 m == 0 = error "Failed"
    |  otherwise          = (joinBlocks (1, zeroes, sMult v (1/ getElem 1 1 a11), l'), joinBlocks (a11, wT, transpose zeroes, u'))   where
    (a11,wT,v,a') = splitBlocks 1 1 m
    -- decompose again by finding the LU decomposition of the schur complement
    (l',u')       = lumDecomp (a'-v* sMult wT (1/ getElem 1 1 a11) )
    zeroes        = zero 1 (ncols a')

-- in format L,U,P
lumDecompPerm :: (Fractional a, Num a, Eq a) =>  Matrix a -> (Matrix a, Matrix a, Matrix a)
lumDecompPerm m
    |  nrows m == 1       = (m, fromList 1 1 [1], fromList 1 1 [1])
    |  getElem 1 1 m == 0 = (\((x1,x2),x3)->(x1,x2,x3)) (calc ((perm m)*m), perm m)
    |  otherwise          =  (\((x1,x2),x3)->(x1,x2,x3)) (calc m, identity (nrows m))

perm :: (Eq a1, Num a2, Num a1) => Matrix a1 -> Matrix a2
perm x = permMatrix (nrows x) 1 (fromJust (findIndex (/= 0) extNum) +1) where
    extNum = toList $ colVector (getCol 1 x)

calc :: (Fractional a, Eq a) => Matrix a -> (Matrix a, Matrix a)
calc x = (joinBlocks (1, zeroes, p' * sMult v (1/ getElem 1 1 a11), l'), joinBlocks (a11, wT, transpose zeroes, u')) where
    (a11,wT,v,a')    = splitBlocks 1 1 x
    (l',u',p')       = lumDecompPerm (a'-v* sMult wT (1/ getElem 1 1 a11) )
    zeroes           = zero 1 (ncols a')


sMult :: Num b => Matrix b -> b -> Matrix b
sMult m x = mapPos (\(r,c) a -> a * x) m

-- helper function to get the size of the matrix


--mydet :: Num a => (Matrix a, Matrix a, c) -> a
--mydet (x,y,z) = (diagProd x)*(diagProd y)*(-1.0) P.**s where
--    s = ((nrows z)-(trace z)) / 2

