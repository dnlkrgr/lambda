# lambda

**lambda** is a small interpreter for the untyped lambda calculus without the Y-combinator.
You can enter lambda expressions which are then evaluated to normal form.

## How to run:

### with nix + cabal:
* `nix-shell --run 'cabal new-run'`

### only cabal:
* `cabal new-run`

### stack:
* `stack run`

## Syntax for lambda expressions:

t := n | x | /x. t | t1 t2 | (t1) (t2)
