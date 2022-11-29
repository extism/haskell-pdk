# Haskell PDK

A Haskell library for writing [Extism](https://github.com/extism/extism) plugins

See [examples/](https://github.com/extism/haskell-pdk/tree/main/src/examples) for some example plugins

## Notes

- This library is intended to be used with [wasm32-wasi-ghc](https://gitlab.haskell.org/ghc/ghc-wasm-meta)
- The Haskell PDK is different from the others because it requires WASI and the resulting plugins expose a 
  single `_start` function instead of named functions.  It is possible to export named Haskell functions, 
  however because Haskell has a runtime that needs to be initialized it's not possible to call them directly.
- If you're geting linker errors about undefined Extism functions when compiling a plugin then the following 
  arguments need to be passed to GHC: `-optl -Wl,--allow-undefined`
