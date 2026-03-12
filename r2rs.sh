#!/bin/sh

rm -f r2rs

pushd build/c
make r2rs
mv r2rs ../../
popd

./r2rs
