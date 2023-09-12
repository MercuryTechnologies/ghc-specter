.PHONY: clean
clean:
	cabal clean

.PHONY: build
build:
	cabal build all

.PHONY: format
format:
	find ./daemon -name "*.hs" -exec ormolu -i --no-cabal -o -XGHC2021 {} \; && \
	find ./plugin -name "*.hs" -exec ormolu -i --no-cabal -o -XGHC2021 {} \; && \
	find ./ghc-build-analyzer -name "*.hs" -exec ormolu -i --no-cabal -o -XGHC2021 {} \;
