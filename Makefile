all: hello.example http_get.example count_vowels.example

build:
	wasm32-wasi-cabal build
	
%.example: build
	cp `find dist-newstyle/build/wasm32-wasi/ -name $*.wasm` ./$*.wasm

clean:
	cabal clean