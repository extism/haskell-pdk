# Extism Haskell PDK

For more information about the Haskell PDK, please [visit the docs](https://extism.org/docs/write-a-plugin/haskell-pdk).

See [examples/](https://github.com/extism/haskell-pdk/tree/main/examples) for some example plugins

Join the [Discord](https://discord.gg/cx3usBCWnc) and chat with us!

## Usage

- This library is intended to be used with [wasm32-wasi-ghc](https://gitlab.haskell.org/ghc/ghc-wasm-meta)
- The Haskell PDK is different from the other PDKs because it requires WASI and the resulting plugins expose a 
  single `_start` function instead of named functions.  It is possible to export named Haskell functions, 
  however because Haskell has a runtime that needs to be initialized it's not possible to call them directly.
- If you're geting linker errors about undefined Extism functions when compiling a plugin then the following 
  arguments need to be passed to GHC: `-optl -Wl,--allow-undefined`
  (see [extism-pdk.cabal](https://github.com/extism/haskell-pdk/tree/main/extism-pdk.cabal))