#!/bin/bash

TAGGER="python tagger.py"
THORN="src/thorn.opt"

FORMAT=${1:-"deriv"}

MODEL=${2:-"models/tri_headfirst"}

SEEDFILE=${3:-"ccg.seeds"}

BATCHSIZE=${5:-16}

NBEST=1

cat - | \
  sed -f tokenizer.sed | \
  sed 's/ _ /_/g' \ |
  $TAGGER --out $SEEDFILE --batchsize $BATCHSIZE $MODEL

$THORN -nbest $NBEST -format $FORMAT $SEEDFILE $MODEL
