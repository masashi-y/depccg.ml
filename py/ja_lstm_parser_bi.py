
from __future__ import print_function
import sys
import os
import numpy as np
import json
import chainer
import chainer.links as L
import chainer.functions as F
from chainer import cuda
from chainer import training, Variable
from chainer.training import extensions
from chainer.optimizer import WeightDecay, GradientClipping
from py.japanese_ccg import JaCCGReader
from collections import defaultdict, OrderedDict
from py.py_utils import read_pretrained_embeddings, read_model_defs
from py.tree import Leaf, Tree, get_leaves
from py.biaffine import Biaffine
from py.param import Param

UNK = "*UNKNOWN*"
START = "*START*"
END = "*END*"
IGNORE = -1

def log(args, out):
    for k, v in vars(args).items():
        out.write("{}: {}\n".format(k, v))

class TrainingDataCreator(object):
    """
    create train & validation data
    """
    def __init__(self, filepath, word_freq_cut, char_freq_cut, cat_freq_cut):
        self.filepath = filepath
         # those categories whose frequency < freq_cut are discarded.
        self.word_freq_cut = word_freq_cut
        self.char_freq_cut = char_freq_cut
        self.cat_freq_cut  = cat_freq_cut
        self.seen_rules = defaultdict(int) # seen binary rules
        self.unary_rules = defaultdict(int) # seen unary rules
        self.cats = defaultdict(int) # all cats
        self.words = defaultdict(int)
        self.chars = defaultdict(int)
        self.samples = OrderedDict()
        self.sents = []

        self.words[UNK]      = 10000
        self.words[START]    = 10000
        self.words[END]      = 10000
        self.chars[UNK]      = 10000
        self.chars[START]    = 10000
        self.chars[END]      = 10000
        self.cats[START]     = 10000
        self.cats[END]       = 10000

    def _traverse(self, tree):
        if isinstance(tree, Leaf):
            self.cats[tree.cat.without_semantics] += 1
            w = tree.word
            self.words[w] += 1
            for c in w:
                self.chars[c] += 1
        else:
            children = tree.children
            if len(children) == 1:
                rule = tree.cat.without_semantics + \
                        " " + children[0].cat.without_semantics
                self.unary_rules[rule] += 1
                self._traverse(children[0])
            else:
                rule = children[0].cat.without_semantics + \
                        " " + children[1].cat.without_semantics
                self.seen_rules[rule] += 1
                self._traverse(children[0])
                self._traverse(children[1])

    @staticmethod
    def _write(dct, out, comment_out_value=False):
        print("writing to", out.name, file=sys.stderr)
        for key, value in dct.items():
            out.write(key.encode("utf-8") + " ")
            if comment_out_value:
                out.write("# ")
            out.write(str(value) + "\n")

    def _get_dependencies(self, tree, sent_len):
        def rec(subtree):
            if isinstance(subtree, Tree):
                children = subtree.children
                if len(children) == 2:
                    head = rec(children[0 if subtree.left_is_head else 1])
                    dep  = rec(children[1 if subtree.left_is_head else 0])
                    res[dep] = head
                else:
                    head = rec(children[0])
                return head
            else:
                return subtree.pos

        res = [-1 for _ in range(sent_len)]
        rec(tree)
        res = [i + 1 for i in res]
        assert len(filter(lambda i:i == 0, res)) == 1
        return res

    def _to_conll(self, out):
        for sent, (cats, deps) in self.samples.items():
            for i, (w, c, d) in enumerate(zip(sent.split(" "), cats, deps), 1):
                out.write("{}\t{}\t{}\t{}\n".format(i, w.encode("utf-8"), c, d))
            out.write("\n")

    def _create_samples(self, trees):
        for tree in trees:
            tokens = get_leaves(tree)
            words = [token.word for token in tokens]
            cats = [token.cat.without_semantics for token in tokens]
            deps = self._get_dependencies(tree, len(tokens))
            sent = " ".join(words)
            self.sents.append(sent)
            self.samples[sent] = cats, deps

    @staticmethod
    def create_traindata(args):
        self = TrainingDataCreator(args.path,
                args.word_freq_cut, args.char_freq_cut, args.cat_freq_cut)
        with open(args.out + "/log_create_traindata", "w") as f:
            log(args, f)

        trees = JaCCGReader(self.filepath).readall()
        for tree in trees:
            self._traverse(tree)
        self._create_samples(trees)

        self.cats = {k: v for (k, v) in self.cats.items() \
                        if v >= self.cat_freq_cut}
        self.words = {k: v for (k, v) in self.words.items() \
                        if v >= self.word_freq_cut}
        self.chars = {k: v for (k, v) in self.chars.items() \
                        if v >= self.char_freq_cut}
        with open(args.out + "/unary_rules.txt", "w") as f:
            self._write(self.unary_rules, f, comment_out_value=True)
        with open(args.out + "/seen_rules.txt", "w") as f:
            self._write(self.seen_rules, f, comment_out_value=True)
        with open(args.out + "/target.txt", "w") as f:
            self._write(self.cats, f, comment_out_value=False)
        with open(args.out + "/words.txt", "w") as f:
            self._write(self.words, f, comment_out_value=False)
        with open(args.out + "/chars.txt", "w") as f:
            self._write(self.chars, f, comment_out_value=False)
        with open(args.out + "/traindata.json", "w") as f:
            json.dump(self.samples, f)
        with open(args.out + "/trainsents.txt", "w") as f:
            for sent in self.sents:
                f.write(sent.encode("utf-8") + "\n")
        with open(args.out + "/trainsents.conll", "w") as f:
            self._to_conll(f)

    @staticmethod
    def create_testdata(args):
        self = TrainingDataCreator(args.path,
                args.word_freq_cut, args.char_freq_cut, args.cat_freq_cut)
        with open(args.out + "/log_create_testdata", "w") as f:
            log(args, f)
        trees = JaCCGReader(self.filepath).readall()
        self._create_samples(trees)
        with open(args.out + "/testdata.json", "w") as f:
            json.dump(self.samples, f)
        with open(args.out + "/testsents.conll", "w") as f:
            self._to_conll(f)
        with open(args.out + "/testsents.txt", "w") as f:
            for sent in self.sents:
                f.write(sent.encode("utf-8") + "\n")


class FeatureExtractor(object):
    def __init__(self, model_path):
        self.model_path = model_path
        self.words = read_model_defs(model_path + "/words.txt")
        self.chars = read_model_defs(model_path + "/chars.txt")
        self.unk_word = self.words[UNK]
        self.start_word = self.words[START]
        self.end_word = self.words[END]
        self.unk_char = self.chars[UNK]
        self.start_char = self.chars[START]
        self.end_char = self.chars[END]

    def process(self, words):
        """
        words: list of unicode tokens
        """
        w = np.array([self.start_word] + [self.words.get(
            x, self.unk_word) for x in words] + [self.end_word], 'i')
        l = max(len(x) for x in words)
        c = -np.ones((len(words) + 2, l), 'i')
        c[0, 0] = self.start_char
        c[-1, 0] = self.end_char
        for i, word in enumerate(words, 1):
            for j in range(len(word)):
                c[i, j] = self.chars.get(word[j], self.unk_char)
        return w, c, np.array([l], 'i')


class LSTMParserDataset(chainer.dataset.DatasetMixin):
    def __init__(self, model_path, samples_path):
        self.model_path = model_path
        self.extractor = FeatureExtractor(model_path)
        self.targets = read_model_defs(model_path + "/target.txt")
        with open(samples_path) as f:
            self.samples = json.load(f).items()

    def __len__(self):
        return len(self.samples)

    def get_example(self, i):
        words, [cats, deps] = self.samples[i]
        words = words.split(" ")
        w, c, l = self.extractor.process(words)
        cats = np.array([self.targets.get(x, IGNORE) \
                for x in [START] + cats + [END]], 'i')
        deps = np.array([-1] + deps + [-1], 'i')
        return w, c, l, cats, deps


class BiaffineJaLSTMParser(chainer.Chain):
    def __init__(self, model_path, word_dim=None, char_dim=None, nlayers=2,
            hidden_dim=128, dep_dim=100, dropout_ratio=0.5):
        self.model_path = model_path
        defs_file = model_path + "/tagger_defs.txt"
        if word_dim is None:
            self.train = False
            Param.load(self, defs_file)
            self.extractor = FeatureExtractor(model_path)
        else:
            self.train = True
            p = Param(self)
            p.dep_dim = dep_dim
            p.word_dim = word_dim
            p.char_dim = char_dim
            p.hidden_dim = hidden_dim
            p.nlayers = nlayers
            p.n_words = len(read_model_defs(model_path + "/words.txt"))
            p.n_chars = len(read_model_defs(model_path + "/chars.txt"))
            p.targets = read_model_defs(model_path + "/target.txt")
            p.dump(defs_file)

        self.in_dim = self.word_dim + self.char_dim
        self.dropout_ratio = dropout_ratio
        super(BiaffineJaLSTMParser, self).__init__(
                emb_word=L.EmbedID(self.n_words, self.word_dim),
                emb_char=L.EmbedID(self.n_chars, 50, ignore_label=IGNORE),
                conv_char=L.Convolution2D(1, self.char_dim,
                    (3, 50), stride=1, pad=(1, 0)),
                lstm_f=L.NStepLSTM(self.nlayers, self.in_dim,
                    self.hidden_dim, 0.32),
                lstm_b=L.NStepLSTM(self.nlayers, self.in_dim,
                    self.hidden_dim, 0.32),
                arc_dep=L.Linear(2 * self.hidden_dim, self.dep_dim),
                arc_head=L.Linear(2 * self.hidden_dim, self.dep_dim),
                rel_dep=L.Linear(2 * self.hidden_dim, self.dep_dim),
                rel_head=L.Linear(2 * self.hidden_dim, self.dep_dim),
                biaffine_arc=Biaffine(self.dep_dim),
                biaffine_tag=L.Bilinear(self.dep_dim, self.dep_dim, len(self.targets))
                )

    def load_pretrained_embeddings(self, path):
        self.emb_word.W.data = read_pretrained_embeddings(path)

    def __call__(self, xs):
        """
        xs [(w,s,p,y), ..., ]
        w: word, c: char, l: length, y: label
        """
        batchsize = len(xs)
        ws, cs, ls, cat_ts, dep_ts = zip(*xs)
        cat_ys, dep_ys = self.forward(ws, cs, ls, dep_ts if self.train else None)

        cat_loss = reduce(lambda x, y: x + y,
            [F.softmax_cross_entropy(y, t) for y, t in zip(cat_ys, cat_ts)])
        cat_acc = reduce(lambda x, y: x + y,
            [F.accuracy(y, t, ignore_label=IGNORE) for y, t in zip(cat_ys, cat_ts)])

        dep_loss = reduce(lambda x, y: x + y,
            [F.softmax_cross_entropy(y, t) for y, t in zip(dep_ys, dep_ts)])
        dep_acc = reduce(lambda x, y: x + y,
            [F.accuracy(y, t, ignore_label=IGNORE) for y, t in zip(dep_ys, dep_ts)])


        cat_acc /= batchsize
        dep_acc /= batchsize
        chainer.report({
            "tagging_loss": cat_loss,
            "tagging_accuracy": cat_acc,
            "parsing_loss": dep_loss,
            "parsing_accuracy": dep_acc
            }, self)
        return cat_loss + dep_loss

    def forward(self, ws, cs, ls, dep_ts=None):
        batchsize = len(ws)
        xp = chainer.cuda.get_array_module(ws[0])
        ws = map(self.emb_word, ws)
        cs = [F.squeeze(
            F.max_pooling_2d(
                self.conv_char(
                    F.expand_dims(
                        self.emb_char(c), 1)), (int(l[0]), 1)))
                    for c, l in zip(cs, ls)]
        xs_f = [F.dropout(F.concat([w, c]),
            self.dropout_ratio, train=self.train) for w, c in zip(ws, cs)]
        xs_b = [x[::-1] for x in xs_f]
        cx_f, hx_f, cx_b, hx_b = self._init_state(xp, batchsize)
        _, _, hs_f = self.lstm_f(hx_f, cx_f, xs_f, train=self.train)
        _, _, hs_b = self.lstm_b(hx_b, cx_b, xs_b, train=self.train)
        hs_b = [x[::-1] for x in hs_b]
        hs = [F.concat([h_f, h_b]) for h_f, h_b in zip(hs_f, hs_b)]


        dep_ys = [self.biaffine_arc(
            F.elu(F.dropout(self.arc_dep(h), 0.32, train=self.train)),
            F.elu(F.dropout(self.arc_head(h), 0.32, train=self.train))) for h in hs]

        if dep_ts is not None:
            heads = dep_ts
        else:
            heads = [F.argmax(y, axis=1) for y in dep_ys]

        cat_ys = [
                self.biaffine_tag(
            F.elu(F.dropout(self.rel_dep(h), 0.32, train=self.train)),
            F.elu(F.dropout(self.rel_head(
                F.embed_id(t, h, ignore_label=IGNORE)), 0.32, train=self.train))) \
                        for h, t in zip(hs, heads)]

        return cat_ys, dep_ys

    def predict(self, xs):
        """
        batch: list of splitted sentences
        """
        xs = [self.extractor.process(x) for x in xs]
        ws, ss, ps = zip(*xs)
        cat_ys, dep_ys = self.forward(ws, ss, ps)
        return zip([F.log_softmax(y[1:-1]).data for y in cat_ys],
                [F.log_softmax(y[1:-1, :-1]).data for y in dep_ys])

    def predict_doc(self, doc, batchsize=16):
        """
        doc list of splitted sentences
        """
        res = []
        for i in range(0, len(doc), batchsize):
            res.extend([(i + j, 0, y)
                for j, y in enumerate(self.predict(doc[i:i + batchsize]))])
        return res

    def _init_state(self, xp, batchsize):
        res = [Variable(xp.zeros(( # forward cx, hx, backward cx, hx
                self.nlayers, batchsize, self.hidden_dim), 'f')) for _ in range(4)]
        return res

    @property
    def cats(self):
        return zip(*sorted(self.targets.items(), key=lambda x: x[1]))[0]


def converter(xs, device):
    if device is None:
        return xs
    elif device < 0:
        return map(lambda x: map(lambda m: cuda.to_cpu(m), x), xs)
    else:
        return map(lambda x: map(
            lambda m: cuda.to_gpu(m, device, cuda.Stream.null), x), xs)


def train(args):
    model = BiaffineJaLSTMParser(args.model, args.word_emb_size, args.char_emb_size,
            args.nlayers, args.hidden_dim, args.dep_dim, args.dropout_ratio)
    with open(args.model + "/params", "w") as f: log(args, f)

    if args.initmodel:
        print('Load model from', args.initmodel)
        chainer.serializers.load_npz(args.initmodel, model)

    if args.pretrained:
        print('Load pretrained word embeddings from', args.pretrained)
        model.load_pretrained_embeddings(args.pretrained)

    if args.gpu >= 0:
        chainer.cuda.get_device(args.gpu).use()
        model.to_gpu()

    train = LSTMParserDataset(args.model, args.train)
    train_iter = chainer.iterators.SerialIterator(train, args.batchsize)
    val = LSTMParserDataset(args.model, args.val)
    val_iter = chainer.iterators.SerialIterator(
            val, args.batchsize, repeat=False, shuffle=False)
    optimizer = chainer.optimizers.Adam(beta2=0.9)
    # optimizer = chainer.optimizers.MomentumSGD(momentum=0.7)
    optimizer.setup(model)
    optimizer.add_hook(WeightDecay(2e-6))
    # optimizer.add_hook(GradientClipping(5.))
    updater = training.StandardUpdater(train_iter, optimizer,
            device=args.gpu, converter=converter)
    trainer = training.Trainer(updater, (args.epoch, 'epoch'), args.model)

    val_interval = 1000, 'iteration'
    log_interval = 200, 'iteration'

    eval_model = model.copy()
    eval_model.train = False

    trainer.extend(extensions.ExponentialShift(
                    "eps", .75, 2e-3), trigger=(2500, 'iteration'))
    trainer.extend(extensions.Evaluator(
        val_iter, eval_model, converter, device=args.gpu), trigger=val_interval)
    trainer.extend(extensions.snapshot_object(
        model, 'model_iter_{.updater.iteration}'), trigger=val_interval)
    trainer.extend(extensions.LogReport(trigger=log_interval))
    trainer.extend(extensions.PrintReport([
        'epoch', 'iteration',
        'main/tagging_accuracy', 'main/tagging_loss',
        'main/parsing_accuracy', 'main/parsing_loss',
        'validation/main/tagging_accuracy',
        'validation/main/parsing_accuracy'
    ]), trigger=log_interval)
    trainer.extend(extensions.ProgressBar(update_interval=10))

    trainer.run()


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(
                "CCG parser's LSTM supertag tagger")
    parser.set_defaults(func=lambda _: parser.print_help())
    subparsers = parser.add_subparsers()

    # Creating training data
    parser_c = subparsers.add_parser(
            "create", help="create tagger input data")
    parser_c.add_argument("path",
            help="path to ccgbank data file")
    parser_c.add_argument("out",
            help="output directory path")
    parser_c.add_argument("--cat-freq-cut",
            type=int, default=10,
            help="only allow categories which appear >= freq-cut")
    parser_c.add_argument("--word-freq-cut",
            type=int, default=5,
            help="only allow words which appear >= freq-cut")
    parser_c.add_argument("--char-freq-cut",
            type=int, default=5,
            help="only allow characters which appear >= freq-cut")
    parser_c.add_argument("--mode",
            choices=["train", "test"],
            default="train")

    parser_c.set_defaults(func=
            (lambda args:
                TrainingDataCreator.create_traindata(args)
                    if args.mode == "train"
                else  TrainingDataCreator.create_testdata(args)))

            #TODO updater
    # Do training using training data created through `create`
    parser_t = subparsers.add_parser(
            "train", help="train supertagger model")
    parser_t.add_argument("model",
            help="path to model directory")
    parser_t.add_argument("train",
            help="training data file path")
    parser_t.add_argument("val",
            help="validation data file path")
    parser_t.add_argument("--gpu", type=int, default=-1,
            help="path to model directory")
    parser_t.add_argument("--batchsize",
            type=int, default=16, help="batch size")
    parser_t.add_argument("--epoch",
            type=int, default=20, help="epoch")
    parser_t.add_argument("--word-emb-size",
            type=int, default=50,
            help="word embedding size")
    parser_t.add_argument("--char-emb-size",
            type=int, default=32,
            help="character embedding size")
    parser_t.add_argument("--nlayers",
            type=int, default=1,
            help="number of layers for each LSTM")
    parser_t.add_argument("--hidden-dim",
            type=int, default=128,
            help="dimensionality of hidden layer")
    parser_t.add_argument("--dep-dim",
            type=int, default=100,
            help="dim")
    parser_t.add_argument("--dropout-ratio",
            type=float, default=0.5,
            help="dropout ratio")
    parser_t.add_argument("--initmodel",
            help="initialize model with `initmodel`")
    parser_t.add_argument("--pretrained",
            help="pretrained word embeddings")
    parser_t.set_defaults(func=train)

    args = parser.parse_args()
    args.func(args)
