all: hello.example http_get.example count_vowels.example

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build
	
%.example: build
	cp `find dist-newstyle/build/wasm32-wasi/ -name $*.wasm` ./$*.wasm

clean:
	cabal clean

publish: clean
	cabal v2-haddock --haddock-for-hackage
	cabal sdist

