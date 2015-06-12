all: cabal haddock repl

configure:
	cabal configure

cabal:
	cabal install

haddock: configure
	cabal haddock

build: cabal haddock

repl: 
	cabal repl

clean:
	rm -rf ./dist/
