{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Day3 where

import GHC.TypeLits
import Parse

-- run this in ghci, supplying input as a Symbol. make sure it ends in a newline
-- :k! PartOne "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n"

-- Parsing is super slow on large inputs, so maybe parse the bits to a [[Nat]] manually for full-sized input

type family Parse
  (chars :: [Symbol])
  (acc :: [Nat])
  (result :: [[Nat]])
    :: [[Nat]] where
  Parse '[] _ res = res
  Parse ("\n" ': rest) acc res = Parse rest '[] (acc ': res)
  Parse ("0" ': rest) acc res = Parse rest (0 ': acc) res
  Parse ("1" ': rest) acc res = Parse rest (1 ': acc) res

type family EmptyAcc
  (length :: Nat)
    :: [(Nat, Nat)] where
  EmptyAcc 0 = '[]
  EmptyAcc n = '(0, 0) ': EmptyAcc (n - 1)

type family CounterToBits
  (counter :: [(Nat, Nat)])
    :: [Nat] where
  CounterToBits '[] = '[]
  CounterToBits ('(a, b) ': rest) = BiggerCount (CmpNat a b) : CounterToBits rest

type family BiggerCount
  (o :: Ordering)
    :: Nat where
  BiggerCount 'LT = 1
  BiggerCount 'GT = 0

type family PartOne
  (input :: Symbol)
    :: Nat where
  PartOne input = PowerConsumption (Reverse (CounterToBits (Counter (Parse (ToList input) '[] '[]) (EmptyAcc 12))) '[])

type family PowerConsumption
  (gamma :: [Nat])
    :: Nat where
  PowerConsumption gamma = ToDecimal gamma 0 * ToDecimal (Invert gamma) 0

type family ToDecimal
  (bits :: [Nat])
  (acc :: Nat)
    :: Nat where
  ToDecimal '[] acc = acc
  ToDecimal (0 ': rest) acc = ToDecimal rest (2 * acc)
  ToDecimal (1 ': rest) acc = ToDecimal rest (2 * acc + 1)

type family Reverse
  (input :: [Nat])
  (acc :: [Nat])
    :: [Nat] where
  Reverse '[] acc = acc
  Reverse (x ': xs) acc = Reverse xs (x ': acc)

type family Counter
  (input :: [[Nat]])
  (acc :: [(Nat, Nat)])
    :: [(Nat, Nat)] where
  Counter '[] acc = acc
  Counter (bits ': rest) acc = Counter rest (Combine bits acc)

type family Combine
  (bits :: [Nat])
  (acc :: [(Nat, Nat)])
    :: [(Nat, Nat)] where
  Combine '[] '[] = '[]
  Combine (bit ': bits) (acc ': accRest) = CountBit bit acc ': Combine bits accRest

type family CountBit
  (bit :: Nat)
  (counter :: (Nat, Nat))
    :: (Nat, Nat) where
  CountBit 0 '(z, o) = '(z + 1, o)
  CountBit 1 '(z, o) = '(z, o + 1)

type family Invert
  (bits :: [Nat])
    :: [Nat] where
  Invert '[] = '[]
  Invert (b ': bs) = Flip b ': Invert bs

type family Flip
  (bit :: Nat)
    :: Nat where
  Flip 1 = 0
  Flip 0 = 1

-- PART TWO

-- :k! PartTwo "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n"

type family PartTwo
  (input :: Symbol)
    :: [[Nat]] where
  PartTwo input = ReverseAll (Parse (ToList input) '[] '[])

type family ReverseAll
  (list :: [[Nat]])
    :: [[Nat]] where
  ReverseAll '[] = '[]
  ReverseAll (x ': xs) = Reverse x '[] ': ReverseAll xs

type family WithFirstBit
  (bit :: Nat)
  (input :: [[Nat]])
    :: [[Nat]] where
  WithFirstBit _ '[] = '[]
  WithFirstBit 1 ((1 ': bits) ': xs) = (1 ': bits) ': WithFirstBit 1 xs
  WithFirstBit 0 ((0 ': bits) ': xs) = (0 ': bits) ': WithFirstBit 0 xs
  WithFirstBit 1 ((0 ': bits) ': xs) = WithFirstBit 1 xs
  WithFirstBit 0 ((1 ': bits) ': xs) = WithFirstBit 0 xs
