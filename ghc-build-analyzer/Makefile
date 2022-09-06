.PHONY: clean
clean:
	rm -rf ./dist-newstyle

.PHONY: build
build:
	hpack
	cabal build

.PHONY: format
format:
	find . -name "*.hs" -exec fourmolu -i -o -XGHC2021 {} \;
