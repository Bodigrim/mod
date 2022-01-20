# 0.0.0.0

* Offshoot of 0.1.2.2, but without `integer-gmp` and `vector` dependencies.
  Provided only for the sake of clients, who use GHC < 9 with `integer-simple`:
  performance is badly affected and there are no `Storable`, `Prim` and `Unbox` instances.

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
