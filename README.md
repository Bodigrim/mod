# mod [![Hackage](https://img.shields.io/hackage/v/mod.svg)](https://hackage.haskell.org/package/mod) [![Stackage LTS](https://www.stackage.org/package/mod/badge/lts)](https://www.stackage.org/lts/package/mod) [![Stackage Nightly](https://www.stackage.org/package/mod/badge/nightly)](https://www.stackage.org/nightly/package/mod)

[Modular arithmetic](https://en.wikipedia.org/wiki/Modular_arithmetic),
promoting moduli to the type level, with an emphasis on performance.
Originally a part of the [arithmoi](https://hackage.haskell.org/package/arithmoi) package.

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
namely `modular`, `modular-arithmetic` and `finite-field`. One can also use `finite-typelits`,
which covers some elementary modular arithmetic as well.
Unfortunately, all of them fall behind
in terms of performance. Here is a brief comparison:

| Discipline  | `mod`  | `modular` | `modular-arithmetic` | `finite-typelits` | `finite-field`
| :---------- | :----: | :-------: | :------------------: | :---------------: | :------------:
| Addition    | Fast   | Slow      | Slow                 | Slow              | Slow
| Small `(*)` | Fast   | Slow      | Slow                 | Slow              | Slow
| Inversion   | Fast   | N/A       | Slow                 | N/A               | Slow
| Power       | Fast   | Slow      | Slow                 | Slow              | Slow
| Overflows   | Safe   | Safe      | Unsafe               | Safe              | Safe

* __Addition.__
  All competing implementations of
  the modular addition involve divisions, while `mod` completely avoids
  this costly operation. This makes a difference even for small numbers;
  e. g., `sum [1..10^7]` becomes 5x faster. For larger integers the speed up
  is even more significant, because the computational complexity of division is not linear.

* __Small `(*)`.__
  When a modulus fits in a machine word (which is quite a common case on 64-bit architectures),
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
  For instance, `20 ^ 2 :: Mod Word8 100` returns `44` instead of the expected `0`.
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
but from `finite-typelits`' viewpoint this is a by-product.

## Citius, altius, fortius!

If you are looking for an ultimate performance
and your moduli fit into `Word`,
try `Data.Mod.Word`,
which is a drop-in replacement of `Data.Mod`,
offering better performance and much less allocations.

## Benchmarks

Here are some relative benchmarks (less is better),
which can be reproduced by running `cabal bench`.

| Discipline  | `Data.Mod.Word`  | `Data.Mod`  | `modular` | `modular-arithmetic` | `finite-typelits` | `finite-field`
| :---------- | :--------------: | :---------: | :-------: | :------------------: | :---------------: | :------------:
| Sum         |   0.25x          |    1x       |  11.4x    |      5.7x            |  8.9x             | 8.6x
| Product     |   0.95x          |    1x       |  9.6x     |      4.8x            |  7.0x             | 7.0x
| Inversion   |   0.95x          |    1x       |  N/A      |      2.6x            |  N/A              | 3.0x
| Power       |   0.90x          |    1x       |  6.9x     |      3.8x            |  5.0x             | 4.9x

## What's next?

This package was cut out of [`arithmoi`](https://hackage.haskell.org/package/arithmoi)
to provide modular arithmetic
with a light dependency footprint. This goal certainly limits the scope of the API
to the bare minimum. If you need more advanced tools
(the Chinese remainder theorem, cyclic groups, modular equations, etc.)
please refer to the [Math.NumberTheory.Moduli](https://hackage.haskell.org/package/arithmoi/docs/Math-NumberTheory-Moduli.html) module.
