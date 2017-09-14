
from __future__ import print_function
from py.japanese_ccg import JaCCGReader
from py.tree import Tree
import sys

path = sys.argv[1]

trees = JaCCGReader(path).readall()

def collect_dependencies(tree):
    def __terminals(subtree):
        if isinstance(subtree, Tree):
            for child in subtree.children:
                __terminals(child)
        else:
            terminals.append(subtree.word)

    terminals = []
    __terminals(tree)

    def rec(subtree):
        if isinstance(subtree, Tree):
            children = subtree.children
            if len(children) == 2:
                head = rec(children[1])
                dep  = rec(children[0])
                res[dep] = head
            else:
                head = rec(children[0])
            return head
        else:
            return subtree.pos

    idx = range(1, len(terminals) + 1)
    res = [-1 for _ in idx]
    rec(tree)
    res = zip(idx, terminals, [i + 1 for i in res])
    return res

for tree in trees:
    for (i, w, h) in collect_dependencies(tree):
        print("{0}\t{1}\t{1}\t_\t_\t_\t{2}\tnone\t_\t_".\
                format(i, w.encode("utf-8"), h))
    print()

