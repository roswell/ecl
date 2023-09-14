#!/bin/zsh

rm -f nucl

pushd build/c
make nucl
mv nucl ../../
popd

./nucl
