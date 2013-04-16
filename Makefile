CABAL=~/.cabal/bin/cabal-dev

all: local

local:
	$(CABAL) install -Wall -Werror --prefix=$(HOME) --user

sdist:
	$(CABAL) sdist

check:
	$(CABAL) check

build:
	$(CABAL) build -Wall -Werror

clean:
	$(CABAL) clean
