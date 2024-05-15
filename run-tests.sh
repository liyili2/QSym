#!/bin/sh

cabal test --test-show-detail=streaming "$@"
