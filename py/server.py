
from time import sleep
from ccg_seed_pb2 import *
from py.lstm_parser_bi_fast import FastBiaffineLSTMParser
from google.protobuf.json_format import *
import numpy as np
import os, sys, chainer, argparse, socket, struct


def log(msg):
    print("[server]", msg, file=sys.stderr)


def send_msg(sock, msg):
    msg = struct.pack('>I', len(msg)) + msg
    sock.sendall(msg)


def recv_msg(sock):
    raw_msglen = recvall(sock, 4)
    if not raw_msglen:
        return None
    msglen = struct.unpack('>I', raw_msglen)[0]
    return recvall(sock, msglen)


def recvall(sock, n):
    data = b''
    while len(data) < n:
        packet = sock.recv(n - len(data))
        if not packet:
            return None
        data += packet
    return data


def daemonize():
    def fork():
        if os.fork():
            sys.exit()

    def throw_away_io():
        stdin = open(os.devnull, 'rb')
        stdout = open(os.devnull, 'ab+')
        stderr = open(os.devnull, 'ab+', 0)

        for (null_io, std_io) in zip((stdin, stdout, stderr),
                             (sys.stdin, sys.stdout, sys.stderr)):
            os.dup2(null_io.fileno(), std_io.fileno())

    fork()
    os.setsid()
    fork()
    throw_away_io()


def create_seeds(sents, tagging_result, categories):
    seeds = [None for _ in range(len(sents))]
    for (i, _, (cat_probs, dep_probs)) in tagging_result:
        sentence = sents[i]
        seed = CCGSeed()
        seed.id = i
        seed.sentence.extend(sentence)
        seed.cat_probs.values.extend(cat_probs.flatten().astype(float).tolist())
        seed.cat_probs.shape.extend(list(cat_probs.shape))
        seed.dep_probs.values.extend(dep_probs.flatten().astype(float).tolist())
        seed.dep_probs.shape.extend(list(dep_probs.shape))
        seeds[i] = seed
    seeds = CCGSeeds(lang="en", categories=categories, seeds=seeds)
    return seeds


parser = argparse.ArgumentParser("LSTM supertag tagger")
parser.add_argument("path", help="path to model directory")
parser.add_argument("--batchsize", type=int,
        default=32, help="batch size")
parser.add_argument("--filename", type=str,
        default="/tmp/tagging_server", help="unix domain socket")
args = parser.parse_args()


with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
    s.bind(args.filename)

    daemonize()

    model = os.path.join(args.path, "tagger_model")
    tagger = FastBiaffineLSTMParser(args.path)

    if os.path.exists(model):
        chainer.serializers.load_npz(model, tagger)
    else:
        sys.exit("Not found: %s" % model)

    while True:
        log('listening')
        s.listen(5)
        c, addr = s.accept()
        log('receiving')
        msg = recv_msg(c)
        log(b"received:" + msg)
        sents = [s.split(' ') for s in msg.decode("utf-8").split('|')]
        res = tagger.predict_doc(sents, args.batchsize)
        seeds = create_seeds(sents, res, tagger.cats)
        while True:
            log('sending')
            try:
                send_msg(c, seeds.SerializeToString())
            except:
                break
            sleep(1)
        c.close()

