#!/bin/sh

cabal new-build "$@" && cabal new-run "$@"
