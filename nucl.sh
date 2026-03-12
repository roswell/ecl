#!/bin/sh

rm -f nucl

pushd build/c
make nucl
mv nucl ../../
popd

./nucl
