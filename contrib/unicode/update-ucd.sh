#!/bin/bash
set -e
curl https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt -o UnicodeData.txt
curl https://www.unicode.org/Public/UCD/latest/ucd/Jamo.txt -o Jamo.txt
ecl --norc --load ucd.lisp --eval "(slurp-ucd)" --eval "(output-c)" --eval "(output-c t)" --eval "(ext:quit)"
ecl --norc --compile load-names.lisp --compile names-pairs.lisp --load load-names.fas --load names-pairs.fas --load names-pairs-sort.lisp --eval "(ext:quit)"
rm load-names.fas names-pairs.fas
rm UnicodeData.txt Jamo.txt
