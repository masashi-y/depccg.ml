
from time import sleep
from ccg_seed_pb2 import *
from lstm_parser_bi_fast import FastBiaffineLSTMParser
from google.protobuf.json_format import *
import numpy as np
import os, sys, chainer, argparse, socket, struct
from tagger import *
import multiprocessing


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


def on_other_thread(socket, args):
    msg = recv_msg(socket)
    sentences = msg.decode("utf-8").split("\n")
    sentences = [i.split(" ") for i in sentences]
    res = run(sentences, *args)
    res = res.SerializeToString()
    log('sending')
    send_msg(socket, res)
    socket.close()
    log('done')


parser = argparse.ArgumentParser("LSTM supertag tagger")
parser.add_argument("path", help="path to model directory")
parser.add_argument("--batchsize", type=int,
        default=32, help="batch size")
parser.add_argument("--filename", type=str,
        default="/tmp/tagging_server", help="unix domain socket")
parser.add_argument("--annotator", default=None,
        choices=['spacy', 'candc'], help="add pos, lemma and entity layers")
parser.add_argument("--daemon", action="store_true")
args = parser.parse_args()


try:
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        s.bind(args.filename)

        if args.daemon:
            daemonize()

        tagger = load_tagger(args.path)

        if args.annotator is not None:
            annotator = annotators[args.annotator]()
        else:
            annotator = None

        model_args = (None, annotator, tagger, args.batchsize)

        while True:
            log('listening')
            s.listen(5)
            c, addr = s.accept()
            log('receiving')
            multiprocessing.Process(
                    target=on_other_thread, args=(c, model_args)).start()
except KeyboardInterrupt:
    os.unlink(args.filename)
