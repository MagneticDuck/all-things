#!/bin/bash

cabal install --force-reinstalls
hasktags -c ./src/
