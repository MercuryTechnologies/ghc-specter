.PHONY: clean
clean:
	cabal clean

.PHONY: build
build:
	cabal build all

.PHONY: format
format:
	find . -name "*.hs" -exec fourmolu -i -o -XGHC2021 {} \;
