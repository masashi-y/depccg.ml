#!/bin/bash

TAGGER="python tagger.py"
THORN="src/thorn.opt"

INPUT=$1

FORMAT=${2:-"deriv"}

MODEL=${3:-"models/tri_headfirst"}

SEEDFILE=${4:-"ccg.seeds"}

BATCHSIZE=${5:-16}

NBEST=1

cat $INPUT | \
  sed -f tokenizer.sed | \
  sed 's/ _ /_/g' | \
  $TAGGER --out $SEEDFILE --batchsize $BATCHSIZE $MODEL

$THORN -nbest $NBEST -format $FORMAT $SEEDFILE $MODEL
