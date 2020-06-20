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
  --let mysecret = ['c'] :: [Char]
  let mysecret = "hey" :: [Char]
  let v = Vault {threshold = 2, shares = 3, secret = mysecret}

  [s1,s2,s3] <- generateShare v
  result <- decrypt [s1,s2]

 -- print "results"
  print result
