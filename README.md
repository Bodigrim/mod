# mod [![Build Status](https://travis-ci.org/Bodigrim/mod.svg)](https://travis-ci.org/Bodigrim/mod) [![Hackage](http://img.shields.io/hackage/v/mod.svg)](https://hackage.haskell.org/package/mod) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/mod/badge)](https://matrix.hackage.haskell.org/package/mod) [![Stackage LTS](http://stackage.org/package/mod/badge/lts)](http://stackage.org/lts/package/mod) [![Stackage Nightly](http://stackage.org/package/mod/badge/nightly)](http://stackage.org/nightly/package/mod)

[Modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic),
promoting moduli to the type level, with an emphasis on performance.
Originally a part of [arithmoi](https://hackage.haskell.org/package/arithmoi) package.

```haskell
> :set -XDataKinds
> 4 + 5 :: Mod 7
(2 `modulo` 7)
> 4 - 5 :: Mod 7
(6 `modulo` 7)
> 4 * 5 :: Mod 7
(6 `modulo` 7)
> 4 / 5 :: Mod 7
(5 `modulo` 7)
> 4 ^ 5 :: Mod 7
(2 `modulo` 7)
```

## Competitors

There are other Haskell packages, employing the very same idea of moduli on the type level,
namely `modular` and `modular-arithmetic`. One can also use `finite-typelits`,
which covers some elementary modular arithmetic as well.
Unfortunately, all of them fall behind
in terms of performance. Here is a brief comparison:

| Discipline  | `mod`  | `modular` | `modular-arithmetic` | `finite-typelits`
| :---------- | :----: | :-------: | :------------------: | :---------------:
| Addition    | Fast   | Slow      | Slow                 | Slow
| Small `(*)` | Fast   | Slow      | Slow                 | Slow
| Inversion   | Fast   | N/A       | Slow                 | N/A
| Power       | Fast   | Slow      | Slow                 | N/A
| Overflows   | Safe   | Safe      | Unsafe               | Safe

* __Addition.__
  All competing implementations of
  the modular addition involve divisions, while `mod` completely avoids
  this costly operation. It makes difference even for small numbers;
  e. g., `sum [1..10^7]` becomes 5x faster. For larger integers the speed up
  is even more significant, because the computational complexity of division is not linear.

* __Small `(*)`.__
  When a modulo fits a machine word (which is quite a common case on 64-bit architectures),
  `mod` implements the modular multiplication as a couple of CPU instructions
  and neither allocates intermediate arbitrary-precision values,
  nor calls `libgmp` at all. For computations like `product [1..10^7]`
  this gives a 3x boost to performance
  in comparison to other libraries.

* __Inversion.__
  This package relies on `libgmp` for modular inversions.
  Even for small arguments it is about 5x faster than
  the native implementation of modular inversion
  in `modular-arithmetic`.

* __Power.__
  This package relies on `libgmp` for modular exponentiation.
  Even for small arguments it is about 2x faster than competitors.

* __Overflows.__
  At first glance `modular-arithmetic` is more flexible than `mod`,
  because it allows to specify the underlying representation of a modular residue,
  e. g., `Mod Integer 100`, `Mod Int 100`, `Mod Word8 100`. We argue that this is
  a dangerous freedom, vulnerable to overflows.
  For instance, `20 ^ 2 :: Mod Word8 100` returns `44` instead of expected `0`.
  Even less expected is that `50 :: Mod Word8 300` appears to be `6`
  (remember that type-level numbers are always `Natural`).

### What is the difference between `mod` and `finite-typelits`?

`mod` is specifically designed to represent modular residues
for mathematical applications (__wrapping-around__ finite numbers) and
provides modular inversion and exponentiation.

The main focus of `finite-typelits` is on __non-wrapping-around__ finite numbers,
like indices of arrays in `vector-sized`.
It features a `Num` instance only for the sake of overloading numeric literals.
There is no lawful way to define `Num` except modular arithmetic,
but from `finite-typelits` viewpoint this is a by-product.

## Citius, altius, fortius!

If you are looking for an ultimate performance
and your moduli fit into `Word`,
try `Data.Mod.Word`,
which is a drop-in replacement of `Data.Mod`,
but offers almost 3x faster addition,
2x faster multiplication and much less allocations.

## Benchmarks

Here are some relative benchmarks (less is better),
which can be reproduced by running `cabal bench`.

| Discipline  | `Data.Mod.Word`  | `Data.Mod`  | `modular` | `modular-arithmetic` | `finite-typelits`
| :---------- | :--------------: | :---------: | :-------: | :------------------: | :---------------:
| Sum         |   0.4x           |    1x       |  4.2x     |      4.9x            |  2.9x
| Product     |   0.5x           |    1x       |  3.2x     |      4.4x            |  2.8x
| Inversion   |   0.8x           |    1x       |  N/A      |      6.1x            |  N/A
| Power       |   0.9x           |    1x       |  5.4x     |      1.8x            |  N/A

## What's next?

This package was cut out of [`arithmoi`](https://hackage.haskell.org/package/arithmoi)
to provide a modular arithmetic
with a light dependency footprint. This goal certainly limits the scope of API
to the bare minimum. If you need more advanced tools
(the Chinese remainder theorem, cyclic groups, modular equations, etc.)
please refer to [Math.NumberTheory.Moduli](https://hackage.haskell.org/package/arithmoi/docs/Math-NumberTheory-Moduli.html).
