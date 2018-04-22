# depccg.ml :seedling::deciduous_tree::camel:

OCaml implementation for A\* CCG parser in:  
[A\* CCG Parsing with a Supertag and Dependency Factored Model](https://arxiv.org/abs/1704.06936).


### Build

#### Requirements

* Python 3
* OCaml (newer version)
* opam (opam-installer)

```
$ pip install chainer protobuf spacy
$ opam pin add depccg -k git https://github.com/masashi-y/depccg.ml.git
```

### Usage
```
Usage: depccg_en [-help] [-input INPUT]  [-model MODEL]
                  [-annotator ANNOTATOR]  [-nbest NBEST]  [-beta BETA]
                  [-format FORMAT]  [-socket SOCKET]  [-ncores NCORES]
                  [-verbose]

Arguments:

Options:
  -i, -input INPUT         :  input file (txt file or seed file (.seeds)  {none}
  -m, -model MODEL         :  path to model directory  {none}
  -a, -annotator ANNOTATOR :  assign POS, NER-tags and lemma using annotator [candc, spacy]  {none}
  -n, -nbest NBEST         :  output nbest parses  {1}
  -b, -beta BETA           :  beta value for pruning  {1e-08}
  -f, -format FORMAT       :  output format: [auto, deriv, html, ptb, prolog, htmls, conll, xml]  {"auto"}
  -S, -socket SOCKET       :  use socket to contact with tagger  {none}
  -c, -ncores NCORES       :  the number of cores to parallelize A* decoders  {4}
  -v, -verbose             :  show all messages  {false}
  -h, -help                :  show this help message and exit
```

```
$ echo "this is a test example ." | depccg_en -format deriv -annotator spacy
 this        is           a      test  example  .
  NP   (S[dcl]\NP)/NP  NP[nb]/N  N/N      N     .
                                --------------->
                                       N
                      ------------------------->
                                 NP
      ----------------------------------------->
                      S[dcl]\NP
-----------------------------------------------<
                    S[dcl]
--------------------------------------------------<rp>
                      S[dcl]
```

| Parser | labeled F1 | unlabeled F1 |
|:------:|:----------:|:------------:|
|depccg (paper)  | 88.8% | 94.0% |
|depccg.ml| 88.9% | 94.1%|


### Citation

If you make use of this software, please cite the following:

    @inproceedings{yoshikawa:2017acl,
      author={Yoshikawa, Masashi and Noji, Hiroshi and Matsumoto, Yuji},
      title={A* CCG Parsing with a Supertag and Dependency Factored Model},
      booktitle={Proc. ACL},
      year=2017,
    }

### Licence
MIT Licence

### Contact
For questions and usage issues, please contact yoshikawa.masashi.yh8@is.naist.jp .

