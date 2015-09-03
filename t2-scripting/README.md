# T2: Light-weight and type-safe scripting with Haskell



```bash
echo 'main = putStrLn "Hello, world!"' > hello.hs
runhaskell hello.hs
cabal update
cabal install turtle-1.2.1
```

```haskell
import Turtle
```

## Overview of Haskell

- interpreted or compiled (today we try interpreted)
- interpreted version simply compile and run on the fly
- temptation to over-abstract

## First example – example.hs

```bash
emacs example.hs
chmod u+x example.hs
./example.hs
Hello, world!
```

```bash
ghc -O2 example.hs
./example
```

```bash
$ ghci -v0

Prelude> :set -XOverloadedStrings
Prelude> import Turtle
Prelude Turtle> echo "Hello, world!"
Hello, world!
Prelude Turtle> 2 + 2
4
Prelude Turtle> let f x = x + x
Prelude Turtle> f 2
4
Prelude Turtle> :quit
```

```bash
$ ghci -v0 example.hs
*Main> main
Hello, world!
*Main> :quit
```

## ghci as as a shell

`.ghci` file in your current directory

```
:set -XOverloadedStrings
import Turtle
```
then `ghci -v0`

`ghci` auto-print

## Notes

- “return is the only case where (<-) and (=) behave the same way”
- “use `ghci -v0` to work type based”
- “`=>` is a constraint”
- “A `Shell a` is a stream of `a`s”
- “The empty stream paragraph ==== equational reasoning”
- “`inshell` takes a command and feed its output into another command”
- “minMax combine the folds at the same time”
- “tuple constructor: `(,) :: a -> b -> (a, b)`”
- `fold (ls "/tmp") Control.Foldl.length`
- “traverse only once: `>>> fold (select [1..10]) (Fold.length + Fold.sum)`”
- “`<|>` alternative, choose between 2 things”
- https://hackage.haskell.org/package/turtle-1.2.1/docs/Turtle-Options.html
- “use of `optional` and Turtle-Options is pretty cool (e.g. --help)”
- “no verbose type signatures”
- “String and Text similar but different. Text is the 'right' type”
- “Turtle faster than conduit or pipe. Better on open/close resources”
- “for concurrency: `proc` or `shell`”

## Reference

- https://github.com/Gabriel439/Haskell-Turtle-Library/blob/master/slides/slides.md
- https://hackage.haskell.org/package/turtle-1.2.1/docs/Turtle-Prelude.html
- https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#parsing--pretty-printing
- https://hackage.haskell.org/package/unix-compat
