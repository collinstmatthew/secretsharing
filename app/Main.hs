{-# LANGUAGE DataKinds #-}           -- for Nat (lifting integer to type)

module Main where

import Field as F
import Polynomial
import Vault

import Prelude(IO,($),fst,map,print,snd)
import Math
import Data.Matrix as M
import Prelude as P

main :: IO()
main = do
  let mySecret = "Secret SHHH"
  let v = Vault {threshold = 5, shares = 5, secret = mySecret, fieldOne = F.one :: Modp 7919}

  --[s1,s2,s3,s4,s5] <- generateShare v
  s <- generateShare v
  --result <- decrypt [s1,s2]
  let result = decrypt s

 -- print "results"
  print $ decrypt [s1,s2,s3,s4,s5]
