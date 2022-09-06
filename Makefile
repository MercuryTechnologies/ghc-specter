.PHONY: clean
clean:
	rm -rf ./dist-newstyle

.PHONY: build
build:
	cd plugin && hpack && cd ..;             \
	cd daemon && hpack && cd ..;             \
	cd ghc-build-analyzer && hpack && cd ..; \
	cabal build ghc-build-analyzer;          \
	cabal build ghc-specter-plugin;          \
	cabal build ghc-specter-daemon;          \

.PHONY: format
format:
	find . -name "*.hs" -exec fourmolu -i -o -XGHC2021 {} \;
