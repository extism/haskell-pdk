# Extism Haskell PDK

This library can be used to write [Extism Plug-ins](https://extism.org/docs/concepts/plug-in) in Haskell.

Docs are available on Hackage: [https://hackage.haskell.org/package/extism-pdk](https://hackage.haskell.org/package/extism-pdk)

## Install

Make sure you have [wasm32-wasi-ghc](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) installed, then generate an `Executable` project with cabal:

```bash
cabal init
```

**Note**: As of [aa2d85dc](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/commit/aa2d85dccbce5e18a9ce31ac92511dcdd9a95b6c) the Wasm tail-call
proposal is enabled by default. Some Wasm runtimes, like the go-sdk, don't support this yet so it might be necesarry to pin ghc-wasm-meta to
[ada3b8fa](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/commit/ada3b8fa0f763e4dccb2b1f6bbf2518bff2a7c6e), which seems to be the last commit to not
require tail-calls.

Add the library from [Hackage](https://hackage.haskell.org/package/extism-pdk) to your cabal file:

```bash
build-depends: extism-pdk
```

We will also need to add some additional ghc options to expose the correct functions:

```
ghc-options:
  -optl -Wl,--export=greet -optl -Wl,--export=hs_init -optl -Wl,--allow-undefined -no-hs-main -optl-mexec-model=reactor 
```

## Getting Started

The goal of writing an [Extism plug-in](https://extism.org/docs/concepts/plug-in) is to compile your Haskell code to a Wasm module with exported functions that the host application can invoke. The first thing you should understand is creating an export. Let's write a simple program that exports a `greet` function which will take a name as a string and return a greeting string. 

```haskell
{-# LANGUAGE DeriveDataTypeable #-}

module Hello where

import Data.Maybe
import Extism.PDK
import Extism.PDK.JSON

defaultGreeting = "Hello"

greet g n =
  output $ g ++ ", " ++ n

testing = do
  -- Get a name from the Extism runtime
  name <- inputString
  -- Get  configured greeting
  greeting <- getConfig "greeting"
  -- Greet the user, if no greeting is configured then "Hello" is used
  greet (fromMaybe defaultGreeting greeting) name

foreign export ccall "greet" testing :: IO ()
```

This example also shows how to use the `getConfig` function to load runtime configuration values set by the host.

Despite not needing any system access for this plugin, we will still compile it for `wasm32-wasi`, since there is no Haskell compiler targeting `wasm32-unknown-unknown`:

```bash
wasm32-wasi-cabal build
```

This will put your compiled wasm somewhere in the `dist-newstyle` directory:

```bash
cp `find dist-newstyle -name example.wasm` .
```

We can now test it using the [Extism CLI](https://github.com/extism/cli)'s `run`
command:

```bash
extism call ./example.wasm greet --input "Benjamin"
# => Hello, Benjamin!
```

Configure a new greeting we can update the `greeting` config key using the [Extism CLI](https://github.com/extism/cli)'s `--config` option that lets you pass in `key=value` pairs:

```bash
extism call ./example.wasm greet --input "Benjamin" --config greeting="Hi there"
# => Hi there, Benjamin!
```

> **Note**: We also have a web-based, plug-in tester called the [Extism Playground](https://playground.extism.org/)

### More About Exports

For a function to be available from your Wasm plug-in, you will need to add a `foreign export`:

```haskell
foreign export ccall "greet" greet:: IO Int32
```

And there are some flags to make the function public on the linker side:

```
ghc-options:
    -optl -Wl,--export=greet -optl -Wl,--export=hs_init -optl -Wl,--allow-undefined -no-hs-main -optl-mexec-model=reactor 
```

This will export the `greet` function, the `hs_init` function and compile a reactor module instead of a command-style module.

### Primitive Types

A common thing you may want to do is pass some primitive Haskell data back and forth.

```haskell
-- Float
addPi = do
  -- Get float value
  value <- (input :: IO Float)
  output $ value + 3.14
  return 0

-- Integers
sum42 = do
  value <- (input :: IO Int)
  output $ value + 42
  return 0

-- ByteString
processBytes = do
  bytes <- inputByteString
  -- process bytes here
  output bytes
  return 0

-- String
processString = do
  s <- inputString
  output s
  return 0
```

### Json

We provide a [JSON](https://hackage.haskell.org/package/extism-manifest-0.3.0/docs/Extism-JSON.html) class that allows you to pass JSON encoded values into 
and out of plug-in functions:

```haskell
{-# LANGUAGE DeriveDataTypeable #-}

module Add where
import Extism.PDK
import Extism.PDK.JSON

data Add = Add
  { a :: Int,
    b :: Int
  } deriving (Data)

data Sum = Sum { sum :: Int } deriving (Data)

add = do
  value <- input
  output $ JSON $ Sum (a value + b value)
  return 0

foreign export ccall "add" add :: IO Int32
```

## Variables

Variables are another key-value mechanism but it's a mutable data store that
will persist across function calls. These variables will persist as long as the
host has loaded and not freed the plug-in. You can use [getVar](https://hackage.haskell.org/package/extism-pdk-0.2.0.0/docs/Extism-PDK.html#v:getVar) and [setVar](https://hackage.haskell.org/package/extism-pdk/docs/Extism-PDK.html#v:setVar) to manipulate them.

```haskell
count = do
  c <- fromMaybe 0 <$> getVar "count"
  setVar "count" (c + 1)
  output c
  return 0
```

## Logging

Because Wasm modules by default do not have access to the system, printing to stdout won't work (unless you use WASI). Extism provides some simple logging macros that allow you to use the host application to log without having to give the plug-in permission to make syscalls:

```haskell
module Log where
import Extism.PDK
logStuff = do
  logInfo "Some info!"
  logWarn "A warning!"
  logError "An error!" 
  return 0
foreign export ccall "logStuff" logStuff:: IO Int32
```

From [Extism CLI](https://github.com/extism/cli):

```bash
extism call my_plugin.wasm logStuff --log-level=info
2023/09/30 11:52:17 Some info!
2023/09/30 11:52:17 A warning!
2023/09/30 11:52:17 An error!
```

> *Note*: From the CLI you need to pass a level with `--log-level`. If you are running the plug-in in your own host using one of our SDKs, you need to make sure that you call `set_log_file` to `"stdout"` or some file location.

## HTTP

Sometimes it is useful to let a plug-in make HTTP calls.

> **Note**: See [Request](https://hackage.haskell.org/package/extism-pdk/docs/Extism-PDK-HTTP.html#t:Request) docs for more info on the request and response types:

```haskell
 module HTTPGet where

import Data.Int
import Extism.PDK
import Extism.PDK.HTTP
import Extism.PDK.Memory

httpGet = do
  -- Get JSON encoded request from host
  JSON req <- input
  -- Send the request, get a 'Response'
  res <- sendRequest req (Nothing :: Maybe String)
  -- Save response body to memory
  outputMemory (memory res)
  -- Return code
  return 0

foreign export ccall "httpGet" httpGet :: IO Int32
```

## Imports (Host Functions)

Like any other code module, Wasm not only let's you export functions to the outside world, you can
import them too. Host Functions allow a plug-in to import functions defined in the host. For example,
if you host application is written in Python, it can pass a Python function down to your Haskell plug-in
where you can invoke it.

This topic can get fairly complicated and we have not yet fully abstracted the Wasm knowledge you need
to do this correctly. So we recommend reading out [concept doc on Host Functions](https://extism.org/docs/concepts/host-functions) before you get started.

### A Simple Example

Host functions in the Haskell PDK require C stubs to import a function from a particular namespace:

```c
#define IMPORT(a, b) __attribute__((import_module(a), import_name(b)))
IMPORT("extism:host/user", "a_python_func")
uint64_t a_python_func_impl(uint64_t input);

uint64_t a_python_func(uint64_t input) {
  return a_python_func_impl(input);
}
```

This C file should be added to the `extra-source-files` and `c-sources` fields in your cabal file.

From there we can use `foreign import ccall` to call our stub:

```haskell
import Extism.PDK.Memory
import Extism.PDK

foreign import ccall "a_python_func" aPythonFunc :: Word64 -> IO Word64

helloFromPython :: String -> IO String
helloFromPython = do
  s' <- allocString "Hello!"
  res <- aPythonFunc (memoryOffset s')
  logInfo <$> loadString res
  return 0

foreign export ccall "helloFromPython" helloFromPython :: IO Int32
```

To call this function, we write our input string into memory using `allocString` and call the function with the returned memory handle. We then have
to load the result string from memory to access it from our Haskell program.

### Testing it out

We can't really test this from the Extism CLI as something must provide the implementation. So let's
write out the Python side here. Check out the [docs for Host SDKs](https://extism.org/docs/concepts/host-sdk) to implement a host function in a language of your choice.

```python
from extism import host_fn, Plugin

@host_fn()
def a_python_func(input: str) -> str:
    # just printing this out to prove we're in Python land
    print("Hello from Python!")

    # let's just add "!" to the input string
    # but you could imagine here we could add some
    # applicaiton code like query or manipulate the database
    # or our application APIs
    return input + "!"
```

Now when we load the plug-in we pass the host function:
 
```python
manifest = {"wasm": [{"path": "/path/to/plugin.wasm"}]}
plugin = Plugin(manifest, functions=[a_python_func], wasi=True)
result = plugin.call('helloFromPython', b'').decode('utf-8')
print(result)
```

```bash
python3 app.py
# => Hello from Python!
# => An argument to send to Python!
```

### Reach Out!

Have a question or just want to drop in and say hi? [Hop on the Discord](https://extism.org/discord)!
