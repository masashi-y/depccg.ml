#!/usr/bin/python

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
morpha_cmd = "echo \"{0}\" | java -jar lemmatizer/stemmer.jar"

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

        # use morpha stemmer
        command = morpha_cmd.format(sentence)
        proc = subprocess.Popen(command,
                        shell  = True,
                        stdin  = subprocess.PIPE,
                        stdout = subprocess.PIPE,
                        stderr = subprocess.PIPE)

        res, _ = proc.communicate() 
        res = res.decode('utf-8')
        self.lemmas = [x for x in res.strip().split(" ")]


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
            return x.ent_iob_ + '-' + x.ent_type_


def annotate_one(annotator, sentence):
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


def run(sentences, names, annotator, tagger, batchsize):
    log("tagging")
    res = tagger.predict_doc(sentences, batchsize)
    log("done")
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
            seed.attribs.extend(annotate_one(annotator, sentence))
        seeds[i] = seed
    seeds = CCGSeeds(lang="en", categories=tagger.cats, seeds=seeds)
    return seeds


def load_tagger(filepath):
    model = os.path.join(filepath, "tagger_model")
    with open(os.path.join(filepath, "tagger_defs.txt")) as f:
        tagger = eval(json.load(f)["model"])(filepath)

    if os.path.exists(model):
        chainer.serializers.load_npz(model, tagger)
    else:
        sys.exit("Not found: %s" % model)
    return tagger

if __name__ == '__main__':
    parser = argparse.ArgumentParser("LSTM supertag tagger")
    parser.add_argument("path", help="path to model directory")
    parser.add_argument("--out", default="ccg.seed", help="output file path")
    parser.add_argument("--batchsize", type=int, default=16, help="batch size")
    parser.add_argument("--annotator", default=None,
            choices=['spacy', 'candc'], help="add pos, lemma and entity layers")
    args = parser.parse_args()

    inputs = [i.strip() for i in sys.stdin]
    if len(inputs[0].split("\t")) > 1:
        names, sentences = zip(*[i.split("\t") for i in inputs])
        sentences = [i.split(" ") for i in sentences]
    else:
        sentences = [i.split(" ") for i in inputs]
        names = None

    tagger = load_tagger(args.path)
    if args.annotator is not None:
        annotator = annotators[args.annotator]()
    else:
        annotator = None

    seeds = run(
            sentences, names, annotator, tagger, args.batchsize)

    # log("writing results to : %s" % args.out)
    with open(args.out, "wb") as f:
        sys.stdout.buffer.write(seeds.SerializeToString())
    # log("done")
