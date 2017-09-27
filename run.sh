#!/bin/bash

TAGGER="python tagger.py"
THORN="src/thorn.opt"

MODEL=${1:-"models/tri_headfirst"}

SEEDFILE=${2:-"ccg.seeds"}

FORMAT=${3:-"deriv"}

BATCHSIZE=${4:-16}

NBEST=1

cat - | $TAGGER --out $SEEDFILE --batchsize $BATCHSIZE $MODEL
$THORN -nbest $NBEST -format $FORMAT $SEEDFILE $MODEL
