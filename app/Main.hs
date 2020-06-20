{-# LANGUAGE DataKinds #-}           -- for Nat (lifting integer to type)

--https://crypto.stackexchange.com/questions/8240/implementing-secret-reconstruction-in-shamirs-secret-sharing/8262#8262

--https://rkrishnan.org/posts/2017-06-20-typesafe-modulus-in-haskell.html

module Main where

import Field
import Polynomial
import Vault

import Prelude(IO,($),fst,map,print,snd)
import Math
import Data.Matrix as M
import Prelude as P

--implement legrenge root finding of polynomial
--then allow a string of secret numbers as different coeffieints and not just s1

--somehow at the end let different types run on them

main :: IO()
main = do

  let p = fromList 3 3 [2,-1,0,-1,2,-1,0,-1,2]
  --let p = fromList 2 2 [2,-1,-1,2]
  --print $ lumDecomp p
--  print $ lumDecompPerm p
--  print "mydeterminant is"
  --let (l,u,perm) =  (lumDecompPerm p)
--  print $ mydet (lumDecompPerm p)

  --let p = fromList 4 4 [0,-1.5,6,3,2,5,9,10,76,9,-1,2,-1,7,-1,2]
  --let p = fromList 2 2 [2,-1,-1,2]
  --print $ lumDecomp p
  --let (l,u,perm) = lumDecompPerm p
  --print $ lumDecompPerm p
  --print $ multStd perm p == multStd l u
  --print $ multStd perm p
  --print $ multStd l u

  let m1 = fromList 2 2 [toMod 5 :: Modp 4483, toMod 5 :: Modp 4483, toMod 1 :: Modp 4483,  toMod 5 :: Modp 4483]

  let m2 = fromList 2 1 [toMod 1 :: Modp 4483, toMod 2 :: Modp 4483  ]

--  print 12
  print $ myDet m1
--  let (l1,u1) = lumDecomp m1


  let mysecret = 1898776
  let v = Vault {threshold = 2, shares = 2, secret = mysecret}

  --create order fuction which operates on Vault and gets the next prime number after mysecret
  -- check the s0 functinon is working and also Intl
  [s1,s2] <- generateShare v
  --let order = ffOrder v
  --let x = map (fst) s
  --let y = map (snd) s
  result <- decrypt [s1,s2]
  --print "results"
  print result
