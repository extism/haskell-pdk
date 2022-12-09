# Extism Haskell PDK

For more information about the Haskell PDK, please [visit the docs](https://extism.org/docs/write-a-plugin/haskell-pdk).

See [examples/](https://github.com/extism/haskell-pdk/tree/main/examples) for some example plugins

Join the [Discord](https://discord.gg/cx3usBCWnc) and chat with us!

## Usage

- This library is intended to be used with [wasm32-wasi-ghc](https://gitlab.haskell.org/ghc/ghc-wasm-meta)
- If you're geting linker errors about undefined Extism functions when compiling a plugin then the following 
  arguments need to be passed to GHC: `-optl -Wl,--allow-undefined`
  (see [cabal.project](https://github.com/extism/haskell-pdk/tree/main/cabal.project))
- Functions can be exported using `foreign export` - for a function named `myFunction` you should also pass the
  following to GHC: `-optl -Wl,--export=myFunction`
