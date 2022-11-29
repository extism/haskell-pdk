all: hello.example http_get.example count_vowels.example

build:
	wasm32-wasi-cabal build
	
%.example: build
	cp dist-newstyle/build/wasm32-wasi/ghc-9.5.20221116/extism-pdk-0.1.0.0/x/$*/build/$*/$* ./$*.wasm

clean:
	cabal clean