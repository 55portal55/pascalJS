#!/bin/sh

# compile the application using gambit scheme. It is straightforward to
# modify the script to handle whichever flavor of scheme you wish to use.

cat gambit-optimize.scm \
  src/stubs.scm \
  src/preliminaries.scm src/linkedlist.scm \
  src/pscl-print.scm src/pscl-lex.scm \
  src/hash.scm src/subrange.scm src/enumeration.scm \
  src/pascal.scm src/run-scheme-pascal.scm \
  > x.scm
gsc -c x.scm
gsc -link x.scm
gcc \
  -O1 \
  -D___SINGLE_HOST \
  -m32 \
  -I/Library/Gambit-C/v4.6.1/include \
  -L/Library/Gambit-C/v4.6.1/lib \
  -lgambc -lm x.c x_.c
mv a.out x # executable in x

# to translate a pascal program to scheme run:
# ./x <x.pas
