#!/bin/sh

rm -f pucl

pushd build/c
make pucl
mv pucl ../../
popd

./pucl
