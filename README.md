# Extism Haskell PDK

This library can be used to write [Extism Plug-ins](https://extism.org/docs/concepts/plug-in) in Haskell.

## Notes

- If you're geting linker errors about undefined Extism functions when compiling a plugin then the following 
  arguments need to be passed to GHC: `-optl -Wl,--allow-undefined`
  (see [cabal.project](https://github.com/extism/haskell-pdk/tree/main/cabal.project))
- Functions can be exported using `foreign export` - for a function named `myFunction` you should also pass the
  following to GHC: `-optl -Wl,--export=myFunction`

## Install

Make sure you have [wasm32-wasi-ghc](https://gitlab.haskell.org/ghc/ghc-wasm-meta) installed, thn generate an `Executable` project with cabal:

```bash
cabal init
```

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

The goal of writing an [Extism plug-in](https://extism.org/docs/concepts/plug-in) is to compile your Haskell code to a Wasm module with exported functions that the host application can invoke. The first thing you should understand is creating an export. Let's write a simple program that exports a `greet` function which will take a name as a string and return a greeting string. For this, we use the `#[plugin_fn]` macro on our exported function:


```haskell
module Example where

import Extism.PDK

makeGreeting g n =
  output $ g ++ ", " ++ n

greet = do
  -- Get a name from the Extism runtime
  name <- inputString
  -- Get  configured greeting
  greeting <- getConfig "greeting"
  -- Greet the user, if no greeting is configured then "Hello" is used
  makeGreeting (fromMaybe defaultGreeting greeting) name
  -- Return code
  return 0

foreign export ccall "greet" greet:: IO Int32
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

A common thing you may want to do is pass some primitive Rust data back and forth.

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
  }
  deriving (Typeable, Data)

data Sum = Sum
  { sum :: Int
  }
  deriving (Typeable, Data)

add = do
  value <- input
  output $ JSON $ Sum (a value + b value)
  return 0

foreign export ccall "add" add :: IO Int32
```

## Variables

Variables are another key-value mechanism but it's a mutable data store that
will persist across function calls. These variables will persist as long as the
host has loaded and not freed the plug-in. You can use [getVar](https://hackage.haskell.org/package/extism-pdk-0.2.0.0/docs/Extism-PDK.html#v:getVar) and [setVar](https://hackage.haskell.org/package/extism-pdk-0.2.0.0/docs/Extism-PDK.html#v:setVar) to manipulate them.

```haskell
count = do
  c <- fromMaybe 0 <$> getVar "count"
  setVar "count" (c + 1)
  output c
  return 0
```

## Logging

Because Wasm modules by default do not have access to the system, printing to stdout won't work (unless you use WASI). Extism provides some simple logging macros that allow you to use the host application to log without having to give the plug-in permission to make syscalls. The primary one is [log!](https://docs.rs/extism-pdk/latest/extism_pdk/macro.log.html) but we also have some convenience macros named by log level:

```rust
#[plugin_fn]
pub fn log_stuff() -> FnResult<()> {
    log!(LogLevel::Info, "Some info!");
    log!(LogLevel::Warn, "A warning!");
    log!(LogLevel::Error, "An error!");

    // optionally you can use the leveled macros: 
    info!("Some info!");
    warn!("A warning!");
    error!("An error!");

    Ok(())
}
```

From [Extism CLI](https://github.com/extism/cli):

```bash
extism call my_plugin.wasm log_stuff --log-level=info
2023/09/30 11:52:17 Some info!
2023/09/30 11:52:17 A warning!
2023/09/30 11:52:17 An error!
```

> *Note*: From the CLI you need to pass a level with `--log-level`. If you are running the plug-in in your own host using one of our SDKs, you need to make sure that you call `set_log_file` to `"stdout"` or some file location.

## HTTP

Sometimes it is useful to let a plug-in make HTTP calls.

> **Note**: See [HttpRequest](https://docs.rs/extism-pdk/latest/extism_pdk/struct.HttpRequest.html) docs for more info on the request and response types:

```rust
#[plugin_fn]
pub fn http_get(Json(req): Json<HttpRequest>) -> FnResult<HttpResponse> {
    let res = http::request::<()>(&req, None)?;
    Ok(res)
}
```

## Imports (Host Functions)

Like any other code module, Wasm not only let's you export functions to the outside world, you can
import them too. Host Functions allow a plug-in to import functions defined in the host. For example,
if you host application is written in Python, it can pass a Python function down to your Rust plug-in
where you can invoke it.

This topic can get fairly complicated and we have not yet fully abstracted the Wasm knowledge you need
to do this correctly. So we recommend reading out [concept doc on Host Functions](https://extism.org/docs/concepts/host-functions) before you get started.

### A Simple Example

Host functions have a similar interface as exports. You just need to declare them as `extern` on the top of your `lib.rs`. You only declare the interface as it is the host's responsibility to provide the implementation:

```rust
#[host_fn]
extern "ExtismHost" {
    fn a_python_func(input: String) -> String; 
}
```

To declare a host function in a specific namespace, pass the module name to the `host_fn` macro:

```rust
#[host_fn("extism:host/user")]
```

> **Note**: The types we accept here are the same as the exports as the interface also uses the [convert crate](https://docs.rs/extism-convert/latest/extism_convert/).

To call this function, we must use the `unsafe` keyword. Also note that it automatically wraps the
function return with a Result in case the call fails.


```rust
#[plugin_fn]
pub fn hello_from_python() -> FnResult<String> {
    let output = unsafe { a_python_func("An argument to send to Python".into())? };
    Ok(output)
}
```

### Testing it out

We can't really test this from the Extism CLI as something must provide the implementation. So let's
write out the Python side here. Check out the [docs for Host SDKs](https://extism.org/docs/concepts/host-sdk) to implement a host function in a language of your choice.

```python
from extism import host_fn, Function, ValType

@host_fn
def a_python_func(plugin, input_, output, _user_data):
    # The plug-in is passing us a string
    input_str = plugin.input_string(input_[0])

    # just printing this out to prove we're in Python land
    print("Hello from Python!")

    # let's just add "!" to the input string
    # but you could imagine here we could add some
    # applicaiton code like query or manipulate the database
    # or our application APIs
    input_str += "!"

    # set the new string as the return value to the plug-in
    plugin.return_string(output[0], input_str)
```

Now when we load the plug-in we pass the host function:
 
```python
functions = [
    Function(
        "a_python_func",
        [ValType.I64],
        [ValType.I64],
        a_python_func,
    )
]

plugin = Plugin(manifest, functions=functions)
result = plugin.call('hello_from_python')
print(result)
```

```bash
python3 app.py
# => Hello from Python!
# => An argument to send to Python!
```

### Reach Out!

Have a question or just want to drop in and say hi? [Hop on the Discord](https://extism.org/discord)!
