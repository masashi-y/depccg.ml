
from ccg_seed_pb2 import *
from py.lstm_parser_bi_fast import FastBiaffineLSTMParser
from py.ja_lstm_parser_bi import BiaffineJaLSTMParser
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
parser.add_argument("--annotation", default=None, help="add pos, lemma and entity layers")
args = parser.parse_args()


possible_annotation_layers = {
    "lemma"  : lambda x: x.lemma_,
    "tag"    : lambda x: x.tag_,
    "pos"    : lambda x: x.tag_,
    "entity" : lambda x: x.ent_iob_ + '_' + x.ent_type_
}

if args.annotation is not None:
    annotation_layers = [a.strip() for a in args.annotation.split(",")]
    assert all(a in possible_annotation_layers for a in annotation_layers), \
            "contains unacceptable annotation layer: {}".format(args.annotation)
    annotation_layers = [(a, possible_annotation_layers[a]) for a in annotation_layers]
    import spacy
    nlp = spacy.load("en")
else:
    annotation_layers = []

model = os.path.join(args.path, "tagger_model")
with open(os.path.join(args.path, "tagger_defs.txt")) as f:
    tagger = eval(json.load(f)["model"])(args.path)

if os.path.exists(model):
    chainer.serializers.load_npz(model, tagger)
else:
    sys.exit("Not found: %s" % model)

log("tagging")
inputs = [i.strip() for i in sys.stdin]
if len(inputs[0].split("\t")) > 1:
    names, sentences = zip(*[i.split("\t") for i in inputs])
    sentences = [i.split(" ") for i in sentences]
else:
    sentences = [i.split(" ") for i in inputs]
    names = None
res = tagger.predict_doc(sentences, args.batchsize)
log("done")
log("writing results to : %s" % args.out)
seeds = [None for _ in range(len(sentences))]
for (i, _, (cat_probs, dep_probs)) in res:
    sentence = sentences[i]
    seed = CCGSeed()
    seed.id = str(i) if names is None else names[i]
    seed.sentence.extend(sentence)
    seed.cat_probs.values.extend(cat_probs.flatten().astype(float).tolist())
    seed.cat_probs.shape.extend(list(cat_probs.shape))
    seed.dep_probs.values.extend(dep_probs.flatten().astype(float).tolist())
    seed.dep_probs.shape.extend(list(dep_probs.shape))
    if len(annotation_layers) > 0:
        preds = nlp(" ".join(sentence))
        attribs = []
        for w in preds:
            attrib = Attribute()
            for attr, f in annotation_layers:
                setattr(attrib, attr, f(w))
            seed.attribs.extend(attrib)
    seeds[i] = seed
seeds = CCGSeeds(lang="en", categories=tagger.cats, seeds=seeds)

with open(args.out, "wb") as f:
    f.write(seeds.SerializeToString())
log("done")
