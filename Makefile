build:
	wasm32-wasi-cabal build
	
hello: build
	cp dist-newstyle/build/wasm32-wasi/ghc-9.5.20221116/extism-pdk-0.1.0.0/x/hello/build/hello/hello ./hello.wasm
