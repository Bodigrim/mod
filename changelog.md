# 0.2.2.0

* Plug a loophole to create `undefined {unMod = 100} :: Mod 1`.
* Fix `^%` when power is `minBound :: Int`.
* Fix `^%` when power is 0 and modulo is 1.
* Fix `Data.Euclidean.{gcd,lcm}` returning invalid inhabitants of `Mod m`.

# 0.2.1.0

* Fix `invertMod (0 :: Mod 1)`.

# 0.2.0.1

* Fix build on `aarch64`.

# 0.2.0.0

* Breaking change: redesign `GcdDomain` and `Euclidean` instances.
* Add `instance Read` and `instance Real`.
* Migrate from `integer-gmp` to `ghc-bignum`.
* Remove `(^) -> (^%)` rewrite rule, it does not fire.
* Plug loopholes to inhabit `Mod 0`.
* Work around performance degradation on ARM.

# 0.1.2.2

* Work around an issue with [`fromIntegral`](https://gitlab.haskell.org/ghc/ghc/-/issues/19411) in GHC 9.0.1.

# 0.1.2.1

* Support `integer-gmp-1.1`.

# 0.1.2.0

* Add `Storable`, `Prim` and `Unbox` instances.

# 0.1.1.0

* Add `Data.Mod.Word`.

# 0.1.0.0

* Initial release
