#!/bin/bash

rm test.o
rm debugger.o

ghc -rtsopts -threaded test.hs
ghc debugger.hs

# run like the following
# GHC_DEBUG_SOCKET=/tmp/ghc-debug ./test +RTS -M1G

# ./debugger
