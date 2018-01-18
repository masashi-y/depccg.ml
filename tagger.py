
from ccg_seed_pb2 import *
from py.lstm_parser_bi_fast import FastBiaffineLSTMParser
from py.ja_lstm_parser_bi import BiaffineJaLSTMParser
from google.protobuf.json_format import *
import numpy as np
import os
import sys
import chainer
import argparse
import subprocess

def log(str):
    print("[tagger] %s" % str, file=sys.stderr)

parser = argparse.ArgumentParser("LSTM supertag tagger")
parser.add_argument("path", help="path to model directory")
parser.add_argument("--out", default="ccg.seed", help="output file path")
parser.add_argument("--batchsize", type=int, default=16, help="batch size")
parser.add_argument("--annotator", default=None,
        choices=['spacy', 'candc'], help="add pos, lemma and entity layers")
args = parser.parse_args()


class Annotator(object):
    def lemma(self, i):
        return self.lemmas[i]

    def tag(self, i):
        return self.tags[i]

    def pos(self, i):
        return self.tags[i]

    def entity(self, i):
        return self.entities[i]


candc_cmd = "echo \"{0}\" | {1}/bin/pos --model {1}/models/pos | {1}/bin/ner --model {1}/models/ner"

class CAndCAnnotator(Annotator):
    def __init__(self):
        import spacy
        self.nlp = spacy.load("en")
        try:
            self.candc_dir = os.environ["CANDC"]
        except:
            sys.exit(
                "please set CANDC environment variable! exiting.")

    def annotate(self, sentence):
        command = candc_cmd.format(sentence, self.candc_dir)
        proc = subprocess.Popen(command,
                        shell  = True,
                        stdin  = subprocess.PIPE,
                        stdout = subprocess.PIPE,
                        stderr = subprocess.PIPE)

        res, _ = proc.communicate() 
        res = res.decode('utf-8')
        _, self.tags, self.entities = \
            zip(*[t.split('|') for t in res.strip().split(" ")])
        preds = self.nlp(sentence)
        self.lemmas = [x.lemma_ for x in preds]


class SpacyAnnotator(Annotator):
    def __init__(self):
        import spacy
        self.nlp = spacy.load("en")

    def annotate(self, sentence):
        """
        sentence: string
        """
        preds = self.nlp(sentence)
        self.lemmas = [x.lemma_ for x in preds]
        self.tags = [x.tag_ for x in preds]
        self.entities = \
            [self.get_entity(x) for x in preds]

    def get_entity(self, x):
        if x.ent_iob_ == 'O':
            return x.ent_iob_
        else:
            return x.ent_iob_ + '_' + x.ent_type_


def annotate(annotator, sentence):
    length = len(sentence)
    annotator.annotate(" ".join(sentence))
    attribs = []
    for i in range(length):
        attrib = Attribute()
        attrib.lemma = annotator.lemma(i)
        attrib.pos = annotator.tag(i)
        attrib.entity = annotator.entity(i)
        attribs.append(attrib)
    return attribs


annotators = {
    'spacy' : SpacyAnnotator,
    'candc' : CAndCAnnotator
}

if args.annotator is not None:
    annotator = annotators[args.annotator]
else:
    annotator = None

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
    if annotator is not None:
        seed.attribs.extend(annotate(annotator, sentence))
    seeds[i] = seed
seeds = CCGSeeds(lang="en", categories=tagger.cats, seeds=seeds)

with open(args.out, "wb") as f:
    f.write(seeds.SerializeToString())
log("done")
