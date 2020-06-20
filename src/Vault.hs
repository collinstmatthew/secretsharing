{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}           -- for Nat (lifting integer to type)
{-# LANGUAGE KindSignatures #-}      -- for (n :: Nat) declaration
{-# LANGUAGE RankNTypes #-}          -- for forall

module Vault where

import qualified Data.Char    as C
import qualified Prelude      as P
import qualified GHC.TypeLits as TL
import qualified Field                  as F

import Polynomial
import System.Random(Random,randomRs,newStdGen)
import Data.List(nub)
import Data.Numbers.Primes(isPrime)
import Data.Proxy (Proxy(..))
import Data.List.Split(chunksOf)

-- Currently have MyDoubleon the right hand side but should be a
data Vault a = Vault { threshold :: P.Int, -- Number of points needed for threshold
                       shares    :: P.Int, -- Total number of shares that can be generated
                       secret    :: a    -- The secret member off Finite field
                     } deriving (P.Show)

data Share a = Share { info  :: a,
                       order :: P.Int
                     } deriving (P.Show)

-- take list of values and puts them into a list of shares
inShare :: [(a, b)] -> P.Int -> P.Int -> [Share [(a, b)]]
inShare lst order' nshares = P.map (\z -> Share { info = z, order = order' }) (chunksOf nshares lst) where

randomlist :: Random a => a -> a -> P.IO [a]
randomlist a b = randomRs (a, b) P.<$> newStdGen

--gets the order of the random field used dependent on the secret
--just constant for now could change in the future
ffOrder :: Vault [C.Char] -> P.Int
ffOrder (Vault threshold shares secret') = 7919

nextPrime :: P.Int -> P.Int
nextPrime n | isPrime n = n   -- don't need to compare to True here
            | P.otherwise = nextPrime (n P.+1)

generateShare :: Vault [C.Char] -> P.IO [Share [(P.Int, P.Int)]]
generateShare vault@(Vault threshold shares secret') = do
         let P.Just someNat = TL.someNatVal (P.toInteger (ffOrder vault))

         case someNat of
             TL.SomeNat (_ :: Proxy n1) -> do
                 -- convert char to a number
                 let secretInt   = P.map C.ord secret'
                     secret      = P.map F.fromInteger secretInt :: [ F.Modp n1]
                     sSize       = P.length secret'

                 randoml <- randomlist 1 (ffOrder vault)
                 -- This vector can't have any duplicates otherwise the algorithm will fail
                 let randoml'   = P.map F.fromInteger randoml
                     xvals      = P.take (shares P.* sSize) (nub randoml')
                     polynomial = Polynomial (secret P.++ P.take (threshold P.* sSize P.-sSize) randoml')

                 let points = P.zip (P.map F.toInteger xvals) (P.map (F.toInteger P.. evaluate polynomial) xvals)

                 P.return P.$ inShare points (ffOrder vault) sSize

decrypt :: P.Monad m => [Share [(P.Int, P.Int)]] -> m [C.Char]
decrypt shares = do
         -- size of the secret
         let sSize = P.length P.$ info (shares P.!!0)
         -- do a check to ensure they all have the same order
         let P.Just someNat = TL.someNatVal (P.toInteger (order (P.head shares)))
         case someNat of
             TL.SomeNat (_ :: Proxy n1) -> do
                 -- do stuff here with the computation
                 let x  = P.map (F.fromInteger P.. P.fst) P.$ P.concat P.$ P.map info shares
                 let y :: [F.Modp n1] = P.map (F.fromInteger P.. P.snd) P.$ P.concat P.$ P.map info shares
                 P.return P.$ P.map (\z -> (C.chr (F.toInteger (coeff z x y)))) [1..sSize]
