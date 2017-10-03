#!/bin/bash

PROGNAME=$0
TAGGER="python tagger.py"
THORN="src/thorn.opt"

FORMAT=deriv
MODEL=models/tri_headfirst
SEEDFILE=ccg.seeds
BATCHSIZE=16
BETA=1.0e-15
NBEST=1

log () {
    echo "[shell]" $1 >&2
}

usage() {
    echo "Usage: $PROGNAME [OPTIONS] FILE"
    echo "  This script is ~."
    echo
    echo "Options:"
    echo "  -h, --help"
    echo "  -f, --format"
    echo "  -m, --model"
    echo "  -s, --seed"
    echo "  -b, --batchsize"
    echo "  -n, --nbest"
    echo "  -t, --tokenize"
    echo "  -v, --beta"
    echo
    exit 1
}

for OPT in "$@"
do
    case $OPT in
        '-h' | '--help' )
            usage
            exit 1
            ;;
        '-f' | '--format' )
            FORMAT=$2
            shift 2
            ;;
        '-m' | '--model' )
            MODEL=$2
            shift 2
            ;;
        '-s' | '--seed' )
            SEEDFILE=$2
            shift 2
            ;;
        '-b' | '--batchsize' )
            BATCHSIZE=$2
            shift 2
            ;;
        '-n' | '--nbest' )
            NBEST=$2
            shift 2
            ;;
        '-v' | '--beta' )
            BETA=$2
            shift 2
            ;;
        '-t' | '--tokenize' )
            TOKENIZE=1
            shift 1
            ;;
    esac
done

INPUT=$1

if [ -z $INPUT ]; then
    usage
    exit 1
fi

case $INPUT in
    *\.seeds )
        log "skipping supertagging"
        SEEDFILE=$INPUT
        ;;
    * )
        log "supertagging"
        log "$TAGGER --out $SEEDFILE --batchsize $BATCHSIZE $MODEL"
        if [ "$TOKENIZE" ]; then
            cat $INPUT | \
              sed -f tokenizer.sed | \
              sed 's/ _ /_/g' | \
            $TAGGER --out $SEEDFILE --batchsize $BATCHSIZE $MODEL
        else
            cat $INPUT |  $TAGGER --out $SEEDFILE --batchsize $BATCHSIZE $MODEL
        fi
        ;;
esac


log "$THORN -beta $BETA -nbest $NBEST -format $FORMAT $SEEDFILE $MODEL"

$THORN -beta $BETA -nbest $NBEST -format $FORMAT $SEEDFILE $MODEL
