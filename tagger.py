
from ccg_seed_pb2 import *
from py.lstm_parser_bi_fast import FastBiaffineLSTMParser
from google.protobuf.json_format import *
import numpy as np
import os
import sys
import chainer
import argparse

def log(str):
    print("[tagger] %s" % str, file=sys.stderr)

parser = argparse.ArgumentParser("LSTM supertag tagger")
parser.add_argument("path", help="path to model directory")
parser.add_argument("--out", default="ccg.seed", help="output file path")
parser.add_argument("--batchsize", type=int, default=16, help="batch size")
args = parser.parse_args()

model = os.path.join(args.path, "tagger_model")
tagger = FastBiaffineLSTMParser(args.path)

if os.path.exists(model):
    chainer.serializers.load_npz(model, tagger)
else:
    sys.exit("Not found: %s" % model)

log("tagging")
sentences = [i.strip().split(" ") for i in sys.stdin]
res = tagger.predict_doc(sentences, args.batchsize)
log("done")

seeds = [None for _ in range(len(sentences))]
for (i, _, (cat_probs, dep_probs)) in res:
    sentence = sentences[i]
    seed = CCGSeed()
    seed.id = i
    seed.sentence.extend(sentence)
    seed.cat_probs.values.extend(cat_probs.flatten().astype(float).tolist())
    seed.cat_probs.shape.extend(list(cat_probs.shape))
    seed.dep_probs.values.extend(dep_probs.flatten().astype(float).tolist())
    seed.dep_probs.shape.extend(list(dep_probs.shape))
    seeds[i] = seed
seeds = CCGSeeds(lang="en", categories=tagger.cats, seeds=seeds)

log("writing results to : %s" % args.out)
with open(args.out, "wb") as f:
    f.write(seeds.SerializeToString())
log("done")
