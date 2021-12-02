{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Day2 where

import GHC.TypeLits

-- run this in ghci, supplying input as a type-level [Direction]:
-- :k! MoveSub 0 0 '[Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]

data Direction
  = Forward Nat
  | Down Nat
  | Up Nat

type family MoveSub
  (horiz :: Nat)
  (depth :: Nat)
  (instructions :: [Direction])
    :: Nat where
  MoveSub horiz depth '[] = horiz * depth
  MoveSub horiz depth (Forward n ': rest) =
    MoveSub (horiz + n) depth rest
  MoveSub horiz depth (Down n ': rest) =
    MoveSub horiz (depth + n) rest
  MoveSub horiz depth (Up n ': rest) =
    MoveSub horiz (depth - n) rest

-- PART TWO
-- :k! MoveSubAim 0 0 0 '[Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]

type family MoveSubAim
  (horiz :: Nat)
  (depth :: Nat)
  (aim :: Nat)
  (instructions :: [Direction])
    :: Nat where
  MoveSubAim horiz depth _ '[] = horiz * depth
  MoveSubAim horiz depth aim (Forward n ': rest) =
    MoveSubAim (horiz + n) (depth + aim * n) aim rest
  MoveSubAim horiz depth aim (Down n ': rest) =
    MoveSubAim horiz depth (aim + n) rest
  MoveSubAim horiz depth aim (Up n ': rest) =
    MoveSubAim horiz depth (aim - n) rest
