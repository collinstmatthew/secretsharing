{-# LANGUAGE ScopedTypeVariables #-}
module Vault where

import qualified Prelude as P
import Polynomial
import Field
import System.Random
import Prelude(Int,Show,IO,(<$>),(++),($),fst,map,return,splitAt,take,zip)

-- Currently have MyDoubleon the right hand side but should be a
data Vault a = Vault {  threshold :: Int, -- Number of points needed for threshold
                        shares    :: Int, -- Total number of shares that can be generated
                        secret    :: a    -- The secret member offinite field
                      } deriving (Show)

randomlist :: Random a => a -> a -> IO [a]
randomlist a b = randomRs (a, b) <$> newStdGen

--generateShare :: (Field a, FField a) => Vault a -> IO [(a, a)]
--generateShare (Vault threshold shares base@(Modp s)) = do
generateShare (Vault threshold shares secret) = do
                     --let secret' = 5
                     randoml <- randomlist 1 (size secret)

                     -- This vector can't have any duplicates otherwise the algorithm will fail
                     let randoml'   = map fromInteger randoml
                         xvals      = take shares randoml'
                         polynomial = Polynomial (secret : P.take (threshold P.-1) randoml')
                     return (zip xvals (map (evaluate polynomial) xvals))

-- Encrypt function
--encrypt :: Field f => (a -> f) -> a -> f
encrypt y x = y x

decrypt :: Field a => [a] -> [a] -> a
decrypt x y = s0 x y
