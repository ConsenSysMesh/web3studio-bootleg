#!/bin/bash

echo 'Removing build directories'
rm -rf packages/bootleg-tokens/build
rm -rf packages/bootleg-app-contracts/build
rm -rf packages/examples/build

echo 'Removing 0x-artifact directories'
find . -type d -name '*0x-artifact*' -prune -exec rm -rf {} \;

echo 'Done!'
