# Advent of Code 2021

Solutions to a few advent of code problems, in Haskell's type system

## A note on parsing

Consider Day 2's solution. Right now, it takes in a type-level `[Direction]` directly, rather than parsing the instructions from a `Symbol`. However, imagine we added the `Parse` type family below. Then, using `Parse (ToList instructions)` (with `ToList` from [symbols](https://github.com/kcsongor/symbols)) would give us a `[Direction]` which we could use in the current solution.

Because of the relative unwieldiness and uninterestingness of writing such parsing code, I've instead opted to just do a couple manual input transformations on each problem to feed them into GHCi as "parsed" types directly.

```hs
type family Parse
  (instructionChars :: [Symbol])
    :: [Direction] where
  Parse '[] = '[]
  Parse ("\n" ': rest) = Parse rest
  Parse ("f" ': "o" ': "r" ': "w" ': "a" ': "r" ': "d" ': " " ': rest) =
    Forward (ParseNat rest) ': Parse rest
  Parse ("u" ': "p" ': " " ': rest) =
    Up (ParseNat rest) ': Parse rest
  Parse ("d" ': "o" ': "w" ': "n" ': " " ': rest) =
    Down (ParseNat rest) ': Parse rest

type family ParseNat (s :: [Symbol]) :: Nat where
  ParseNat s = ParseNatHelper s 0

type family ParseNatHelper (s :: [Symbol]) (n :: Nat) :: Nat where
  ParseNatHelper ("0" ': rest) acc = ParseNatHelper rest (10 * acc + 0)
  ParseNatHelper ("1" ': rest) acc = ParseNatHelper rest (10 * acc + 1)
  ParseNatHelper ("2" ': rest) acc = ParseNatHelper rest (10 * acc + 2)
  ParseNatHelper ("3" ': rest) acc = ParseNatHelper rest (10 * acc + 3)
  ParseNatHelper ("4" ': rest) acc = ParseNatHelper rest (10 * acc + 4)
  ParseNatHelper ("5" ': rest) acc = ParseNatHelper rest (10 * acc + 5)
  ParseNatHelper ("6" ': rest) acc = ParseNatHelper rest (10 * acc + 6)
  ParseNatHelper ("7" ': rest) acc = ParseNatHelper rest (10 * acc + 7)
  ParseNatHelper ("8" ': rest) acc = ParseNatHelper rest (10 * acc + 8)
  ParseNatHelper ("9" ': rest) acc = ParseNatHelper rest (10 * acc + 9)
  ParseNatHelper _ acc = acc
```
