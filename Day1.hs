{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Day1 where

import GHC.TypeLits

-- run this in ghci, supplying input as a [Nat]:
-- :k! NumberOfIncreases '[199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

type family IncreaseAmt (o :: Ordering) :: Nat where
  IncreaseAmt 'LT = 1
  IncreaseAmt 'EQ = 0
  IncreaseAmt 'GT = 0

type family NumberOfIncreases (a :: [Nat]) :: Nat where
  NumberOfIncreases '[] = 0
  NumberOfIncreases '[_] = 0
  NumberOfIncreases (a ': b ': rest) =
    IncreaseAmt (CmpNat a b) + NumberOfIncreases (b ': rest)

-- PART TWO
-- :k! SlidingNumberOfIncreases '[199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

type family SlidingNumberOfIncreases (a :: [Nat]) :: Nat where
  SlidingNumberOfIncreases '[] = 0
  SlidingNumberOfIncreases '[_] = 0
  SlidingNumberOfIncreases '[_, _] = 0
  SlidingNumberOfIncreases '[_, _, _] = 0
  SlidingNumberOfIncreases (a ': b ': c ': d ': rest) =
    IncreaseAmt (CmpNat a d) + SlidingNumberOfIncreases (b ': c ': d ': rest)
