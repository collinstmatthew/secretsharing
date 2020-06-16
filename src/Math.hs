-- constructing a field, end goal is to do LU factorisation and then compute the determinant
module Math where

import Prelude(($), (!!), (++), (.), Int, Show, iterate, mod, quotRem, snd)
import Data.Eq
import Data.Proxy (Proxy(..))

import qualified Data.Matrix as M
import Data.Maybe(fromJust)
import Data.List(findIndex)
import qualified Prelude as P

import qualified Field as F

-- should actually probably creat a show instance for this
-- nieve implementation as a list of lists

-- have to implement custome LU decomposition function as the library one requires the elements
-- of a have to be ordered, I didn't look into the source code but I suspect this is to ensure
-- that when the matrix is pivoted on the largeste element is chosen to ensure stability, as we are -- as we are carrying out these computations in finite fields we do not have to worry about that

-- make specialisation when a is a field so
-- then I can multipl
-- look at what specialise keyword does
-- the problem is that a could be both Field and Num and therefore compiler wouldn't know which
-- one to use
--instance Field a => P.Num (M.Matrix (a))

-- minus two matricies fromeacch other
matMinus :: F.Field a => M.Matrix a -> M.Matrix a -> M.Matrix a
matMinus m1 m2 = M.elementwise (\x y -> x F.- y) m1 m2

matPlus :: F.Field a => M.Matrix a -> M.Matrix a -> M.Matrix a
matPlus m1 m2 = M.elementwise (\x y -> x F.+ y) m1 m2

matMult :: F.Field a => M.Matrix a -> M.Matrix a -> M.Matrix a
matMult m1 m2
  | allSame = M.fromList 1 1 [(M.getElem 1 1 m1) F.* (M.getElem 1 1 m2)]
  | maxVal == n  = (matMult aTop m2) M.<-> (matMult aBottom m2)
  | maxVal == p  = (matMult m1 bLeft) M.<|> (matMult m1 bRight)
  | maxVal == m  = (matMult aLeft bTop) `matPlus` (matMult aRight bBottom)
      where (aTop,aBottom) = splitHor m1
			(aLeft,aRight) = splitVert m1
			(bLeft,bRight) = splitVert m2
			(bTop,bBottom) = splitHor m2
            maxVal        = n `P.max` m `P.max` p
            n             = M.nrows m1
            m             = M.ncols m1
            p             = M.ncols m2
            allSame       = n == m P.&& n == p P.&& n == 1

splitVert :: M.Matrix a -> (M.Matrix a, M.Matrix a)
splitVert m
  | M.nrows m == 1 = (a1,a2)
  | P.otherwise = ( a1 M.<-> a3 , a2 M.<-> a4 )  where
      (a1,a2,a3,a4) = (M.splitBlocks 1 1 m)

splitHor :: M.Matrix a -> (M.Matrix a, M.Matrix a)
splitHor m
  | M.ncols m == 1 = (a1,a3)
  | P.otherwise = ( a1 M.<|> a2 , a3 M.<|> a4 )  where
      (a1,a2,a3,a4) = (M.splitBlocks 1 1 m)

--luDecomp :: Field a => Matrix a -> (Matrix a,Matrix a)
--the Num is neccesry for the 1
--http://ressources.unisciel.fr/algoprog/s00aaroot/aa00module1/res/%5BCormen-AL2011%5DIntroduction_To_Algorithms-A3.pdf page 820
-- return type of the function will be (L,U,P)
lumDecomp :: (F.Field a) =>  M.Matrix a -> (M.Matrix a,M.Matrix a)
lumDecomp m
    |  M.nrows m       == 1      = (m, identity 1)
    |  M.getElem 1 1 m == F.zero = P.error "Failed"
    |  P.otherwise               = (M.joinBlocks (identity 1, zeroes, fMult v (F.one F./ M.getElem 1 1 a11), l'), M.joinBlocks (a11, wT, M.transpose zeroes, u'))   where
    (a11,wT,v,a') = M.splitBlocks 1 1 m
    (l',u')       = lumDecomp (a' `matMinus` (v `matMult` fMult wT (F.one F./ M.getElem 1 1 a11) ))
    zeroes        = zeroMat 1 (M.ncols a')

identity :: F.Field a => Int -> M.Matrix a
identity n = M.matrix n n $ \(i,j) -> if i == j then F.one else F.zero

zeroMat :: F.Field a => Int -> Int -> M.Matrix a
zeroMat n1 n2 = M.matrix n1 n2 $ \(i,j) ->  F.zero


-- in format L,U,P
lumDecompPerm :: (P.Fractional a, P.Num a, Eq a) =>  M.Matrix a -> (M.Matrix a, M.Matrix a, M.Matrix a)
lumDecompPerm m
    |  M.nrows m == 1       = (m, M.fromList 1 1 [1], M.fromList 1 1 [1])
    |  M.getElem 1 1 m == 0 = (\((x1,x2),x3)->(x1,x2,x3)) (calc ((perm m) P.*m), perm m)
    |  P.otherwise          = (\((x1,x2),x3)->(x1,x2,x3)) (calc m, M.identity (M.nrows m))

perm :: (Eq a1, P.Num a2, P.Num a1) => M.Matrix a1 -> M.Matrix a2
perm x = M.permMatrix (M.nrows x) 1 (fromJust (findIndex (/= 0) extNum) P.+1) where
    extNum = M.toList $ M.colVector (M.getCol 1 x)

calc :: (P.Fractional a, Eq a) => M.Matrix a -> (M.Matrix a, M.Matrix a)
calc x = (M.joinBlocks (1, zeroes, p' P.* sMult v (1 P./ M.getElem 1 1 a11), l'), M.joinBlocks (a11, wT, M.transpose zeroes, u')) where
    (a11,wT,v,a')    = M.splitBlocks 1 1 x
    (l',u',p')       = lumDecompPerm (a' P.-v P.* sMult wT (1 P./ M.getElem 1 1 a11) )
    zeroes           = M.zero 1 (M.ncols a')


sMult :: P.Num b => M.Matrix b -> b -> M.Matrix b
sMult m x = M.mapPos (\(r,c) a -> a P.* x) m

fMult :: F.Field b => M.Matrix b -> b -> M.Matrix b
fMult m x = M.mapPos (\(r,c) a -> a F.* x) m
-- helper function to get the size of the matrix


--mydet :: Num a => (Matrix a, Matrix a, c) -> a
--mydet (x,y,z) = (diagProd x)*(diagProd y)*(-1.0) P.**s where
--    s = ((nrows z)-(trace z)) / 2

