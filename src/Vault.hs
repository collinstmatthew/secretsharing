{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}           -- for Nat (lifting integer to type)
{-# LANGUAGE KindSignatures #-}      -- for (n :: Nat) declaration
{-# LANGUAGE RankNTypes #-}          -- for forall

module Vault where

import qualified Data.Char as C

import Polynomial
import Field
import qualified Prelude      as P
import qualified GHC.TypeLits as TL
import System.Random(Random,randomRs,newStdGen)
import Prelude(Int,Show,IO,Maybe(..),(<$>),(++),($),(.),(!!),fst,map,max,return,splitAt,
               take,zip,otherwise,toInteger,head,concat)

import Data.List(nub)
import Data.Numbers.Primes(isPrime)

import Data.Proxy (Proxy(..))

import Data.List.Split(chunksOf)

-- Currently have MyDoubleon the right hand side but should be a
data Vault a = Vault { threshold :: Int, -- Number of points needed for threshold
                       shares    :: Int, -- Total number of shares that can be generated
                       secret    :: a    -- The secret member off Finite field
                     } deriving (Show)

data Share a = Share { info  :: a,
                       order :: Int
                     } deriving (Show)

-- take list of values and puts them into a list of shares
inShare :: [(a, b)] -> Int -> Int -> [Share [(a, b)]]
inShare lst order' nshares = map (\z -> Share { info = z, order = order' }) (chunksOf nshares lst) where

randomlist :: Random a => a -> a -> IO [a]
randomlist a b = randomRs (a, b) <$> newStdGen

--gets the order of the random field used dependent on the secret
--need to change this otherwise obviously could find out secret just from order of FF
ffOrder :: Vault [C.Char] -> Int
--ffOrder (Vault threshold shares secret') = nextPrime $ max ((map C.ord secret')!!0) shares
ffOrder (Vault threshold shares secret') = 7919

nextPrime :: Int -> Int
nextPrime n | isPrime n = n   -- don't need to compare to True here
            | otherwise = nextPrime (n P.+1)

generateShare :: Vault [C.Char] -> IO [Share [(Int, Int)]]
generateShare vault@(Vault threshold shares secret') = do
         let Just someNat = TL.someNatVal (toInteger (ffOrder vault))

         case someNat of
             TL.SomeNat (_ :: Proxy n1) -> do
                 -- convert char to a number
                 let secretInt   = map C.ord secret'
                 --let secret = Modp (C.digitToInt secret') :: Modp n1
                 let secret = map Modp secretInt :: [ Modp n1]

                 let sSize = P.length secret'
                 randoml <- randomlist 1 (ffOrder vault)
                 -- This vector can't have any duplicates otherwise the algorithm will fail
                 let randoml'   = map fromInteger randoml
                     -- this should be take unique
                     -- nub removes duplicate elements from a list
                     xvals      = take (shares P.* sSize) (nub randoml')
                     polynomial = Polynomial (secret P.++ take (threshold P.* sSize P.-sSize) randoml')

                 let points :: [(Int,Int)] = zip (map unMod xvals) (map (unMod . evaluate polynomial) xvals)
                 return $ inShare points (ffOrder vault) sSize

decrypt :: P.Monad m => [Share [(Int, Int)]] -> m [C.Char]
decrypt shares = do
         -- size of the secret
         let sSize = P.length $ info (shares!!0)
         -- do a check to ensure they all have the same order
         let Just someNat = TL.someNatVal (toInteger (order (head shares)))
         case someNat of
             TL.SomeNat (_ :: Proxy n1) -> do
                 -- do stuff here with the computation
                 let x :: [Modp n1] = map (toMod . P.fst) $ concat $ map info shares
                 let y :: [Modp n1] = map (toMod . P.snd) $ concat $ map info shares
                 return P.$ map (\z -> (C.chr (unMod (coeff z x y)))) [1..sSize]
