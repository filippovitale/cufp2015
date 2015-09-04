# An Introduction to Type-Level and Generic Programming in Haskell

```haskell
eq :: A -> A -> Bool
```

- to/from isomorphisms
- lot of choices to make, that is why so many different generic libraries

```
Prelude> :set -XDataKinds
```
```
Prelude> :type False
False :: Bool
Prelude> :kind Int
Int :: *
Prelude> :kind Maybe
Maybe :: * -> *
Prelude> :kind IO
IO :: * -> *
Prelude> :kind [] Int
[] Int :: *
Prelude> :kind [Int]
[Int] :: *
Prelude> :kind (,)
(,) :: * -> * -> *
Prelude> :kind False
False :: Bool
Prelude> :kind []
[] :: * -> *
Prelude> :kind '[]
'[] :: [k]
Prelude> :kind '(:)
'(:) :: k -> [k] -> [k]
Prelude> :kind '[Int, Bool, Char]
'[Int, Bool, Char] :: [*]
```

```
λ> :t True `HCons` HNil
True `HCons` HNil :: HList ‘[Bool]
λ> :t 'x' `HCons` (True `HCons` HNil)
'x' `HCons` (True `HCons` HNil) :: HList ‘[Char, Bool]
```

```
λ> :t K 1 :* K 2 :* Nil
K 1 :* K 2 :* Nil :: Num a => NP (K a) '[x, x1]```
```

```
```
λ> :set -XPolyKinds
λ> :t K 1 :* K 2 :* Nil
K 1 :* K 2 :* Nil
  :: forall (k :: BOX) (x :: k) a (x1 :: k).
     Num a =>
     NP (K a) '[x, x1]
```

```
$ ghci -v0 NP.hs -XDataKinds -XPolyKinds
λ> :kind All Show '[Int, Bool]
All Show '[Int, Bool] :: Constraint
λ> :kind! All Show '[Int, Bool]
All Show '[Int, Bool] :: Constraint
= (Show Int, (Show Bool, ()))
```

```
λ> map_NP (\ (I x) -> Just x) (I (2 :: Int) :* I True :* I 'c' :* Nil)
Just 2 :* (Just True :* (Just 'c' :* Nil))
```

```
λ > pure_NP' Nothing :: NP Maybe '[Int, Char, Bool, Bool, Bool]
Nothing :* (Nothing :* (Nothing :* (Nothing :* (Nothing :* Nil))))
```

```bash
cabal install generics-sop
```

```
from (Node (Leaf 3) (Leaf 4))
```

```
λ > eq' (Node (Leaf 3) (Leaf 4)) (Node (Leaf 3) (Leaf 4 :: Int))
True
```

```
λ > eq' (Node (Leaf 3) (Leaf 4)) (Node (Leaf 3) (Leaf 5 :: Int))
False
```
Check `HasMetadata`

More examples on Hackage.

## References

- http://cufp.org/2015/t6-andres-loh-generic-programming.html
- https://github.com/kosmikus/cufp-gp-tutorial-2015
