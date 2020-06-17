-- constructing a field, end goal is to do LU factorisation and then compute the determinant
module Math where

import Prelude(($), (!!), (++), (.), Int, Show, iterate, mod, quotRem, snd)
import Data.Eq
import Data.Proxy (Proxy(..))

import qualified Data.Matrix as M
import Data.Maybe(fromJust)
import Data.List(findIndex)
import qualified Prelude as P

import qualified Data.Vector as V

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

--https://en.wikipedia.org/wiki/Matrix_multiplication_algorithm

fMult :: F.Field b => M.Matrix b -> b -> M.Matrix b
fMult m x = M.mapPos (\(r,c) a -> a F.* x) m

-- helper function to get the size of the matrix
-- minus two matricies fromeacch other
matMinus :: F.Field a => M.Matrix a -> M.Matrix a -> M.Matrix a
matMinus = M.elementwise (F.-)

matPlus :: F.Field a => M.Matrix a -> M.Matrix a -> M.Matrix a
matPlus = M.elementwise (F.+)

matMult :: F.Field a => M.Matrix a -> M.Matrix a -> M.Matrix a
matMult m1 m2
  | allSame = M.fromList 1 1 [M.getElem 1 1 m1 F.* M.getElem 1 1 m2]
  | maxVal == n  = matMult aTop m2     M.<->    matMult aBottom m2
  | maxVal == p  = matMult m1 bLeft    M.<|>    matMult m1 bRight
  | maxVal == m  = matMult aLeft bTop `matPlus` matMult aRight bBottom
      where (aTop,aBottom) = splitHor m1
            (aLeft,aRight) = splitVert m1
            (bLeft,bRight) = splitVert m2
            (bTop,bBottom) = splitHor m2
            maxVal         = n `P.max` m `P.max` p
            n              = M.nrows m1
            m              = M.ncols m1
            p              = M.ncols m2
            allSame        = n == m P.&& n == p P.&& n == 1

splitVert :: M.Matrix a -> (M.Matrix a, M.Matrix a)
splitVert m
  | M.nrows m == 1 = (a1,a2)
  | P.otherwise = ( a1 M.<-> a3 , a2 M.<-> a4 )  where
      (a1,a2,a3,a4) = M.splitBlocks 1 1 m

splitHor :: M.Matrix a -> (M.Matrix a, M.Matrix a)
splitHor m
  | M.ncols m == 1 = (a1,a3)
  | P.otherwise = ( a1 M.<|> a2 , a3 M.<|> a4 )  where
      (a1,a2,a3,a4) = M.splitBlocks 1 1 m

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

-- to get this working just replace [1] with identity and also the permulation matrix working with a Field
-- in format L,U,P
lumDecompPerm :: F.Field a =>  M.Matrix a -> (M.Matrix a, M.Matrix a, M.Matrix a)
lumDecompPerm m
    |  M.nrows m == 1            = (m, identity 1, identity 1)
    |  M.getElem 1 1 m == F.zero = (\((x1,x2),x3)->(x1,x2,x3)) (calc (matMult (perm m) m), perm m)
    |  P.otherwise               = (\((x1,x2),x3)->(x1,x2,x3)) (calc m, identity (M.nrows m))

perm :: F.Field a  => M.Matrix a -> M.Matrix a
perm x = P.fmap (\x -> if x == 0 then F.zero else F.one)  permRes where
    extNum = M.toList $ M.colVector (M.getCol 1 x)
    permRes = M.permMatrix (M.nrows x) 1 (fromJust (findIndex (/= F.zero) extNum) P.+1)

calc :: F.Field a => M.Matrix a -> (M.Matrix a, M.Matrix a)
calc x = (M.joinBlocks (identity 1, zeroes, p' `matMult` fMult v (F.one F./ M.getElem 1 1 a11), l'), M.joinBlocks (a11, wT, M.transpose zeroes, u')) where
    (a11,wT,v,a')    = M.splitBlocks 1 1 x
    (l',u',p')       = lumDecompPerm (a' `matMinus` (v `matMult` fMult wT (F.one F./ M.getElem 1 1 a11) ))
    zeroes           = zeroMat 1 (M.ncols a')

--counts the number of permutations a permutation matrix described
countPerm :: F.Field a => M.Matrix a -> Int
countPerm m = (M.nrows m P.-nonzero) `P.div` 2 where
    nonzero = V.length (V.filter (== F.one) (M.getDiag m))

myDet :: F.Field a => M.Matrix a -> a
myDet m = diagProd l F.* diagProd u F.* F.power (F.zero F.- F.one) (countPerm p) where
  (l,u,p)    = lumDecompPerm m
  diagProd x = P.foldl (F.*) F.one (M.toList (M.rowVector (M.getDiag x)))
