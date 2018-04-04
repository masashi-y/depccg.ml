#!/bin/bash

PROGNAME=$0
TAGGER="python tagger.py"
# THORN="src/thorn.opt"
THORN="jbuilder exec app/parser/main.exe --"

FORMAT=deriv
MODEL=models/tri_headfirst
SEEDFILE=ccg.seeds
BATCHSIZE=16
BETA=1.0e-15
NBEST=1
LNG="en"
TOKENIZE="false"
ANNO="candc"
log () {
    echo "[shell]" $1 >&2
}

usage() {
    echo "Usage: $PROGNAME [OPTIONS] FILE"
    echo "  Camelthorn: A* CCG parser"
    echo
    echo "Argument:"
    echo "  FILE              : input file (either raw text or seed file (*.seeds)"
    echo "Options:"
    echo "  -f, --format      : output format {deriv,auto,xml,html,ptb,prolog,htmls} [$FORMAT]"
    echo "  -m, --model       : path to model directory [$MODEL]"
    echo "  -s, --seed        : seed file the supertagger outputs [$SEEDFILE]"
    echo "  -b, --batchsize   : batch size in tagger [$BATCHSIZE]"
    echo "  -n, --nbest       : number of parses for each sentence [$NBEST]"
    echo "  -t, --tokenize    : apply tokenization (only English) [$TOKENIZE]"
    echo "  -l, --lang        : parse english or japanese sentences {en, ja} [$LNG]"
    echo "  -v, --beta        : beta value used in pruning [$BETA]"
    echo "  -a, --annotator   : annotate pos, entity, lemmas using {candc,spacy} [$ANNO]"
    echo "  -h, --help        : show this text"
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
        '-a' | '--annotator' )
            ANNO=$2
            shift 2
            ;;
        '-l' | '--lang' )
            LNG=$2
            shift 2
            ;;
        '-t' | '--tokenize' )
            TOKENIZE="true"
            shift 1
            ;;
    esac
done

INPUT=$1

if [ -z $INPUT ]; then
    INPUT=/dev/stdin
    # usage
    # exit 1
fi

if [ "$FORMAT" = "xml" ] || [ "$FORMAT" = "prolog" ]; then
    ANNOTATOR="--annotator $ANNO"
fi

case $INPUT in
    *\.seeds | *\.seed )
        log "skipping supertagging"
        SEEDFILE=$INPUT
        ;;
    * )
        log "supertagging"
        log "$TAGGER --out $SEEDFILE --batchsize $BATCHSIZE $ANNOTATOR $MODEL"
        if [ "$TOKENIZE" = "true" ] && [ "$LNG" = "en" ]; then
            log "tokenizing the input texts"
            cat $INPUT | \
              sed -f tokenizer.sed | \
              sed 's/ _ /_/g' | \
            $TAGGER --out $SEEDFILE --batchsize $BATCHSIZE $ANNOTATOR $MODEL
            stat=$?
        else
            cat $INPUT |  $TAGGER --out $SEEDFILE --batchsize $BATCHSIZE $ANNOTATOR $MODEL
            stat=$?
        fi
        if [ $stat != 0 ]; then
          log "some error occurred in tagging. exit."
          exit 1
        fi
        ;;
esac


log "$THORN -beta $BETA -nbest $NBEST -format $FORMAT -lang $LNG $SEEDFILE $MODEL"

$THORN -beta $BETA -nbest $NBEST -format $FORMAT -lang $LNG $SEEDFILE $MODEL
