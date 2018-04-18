# depccg.ml :seedling::deciduous_tree::camel:

OCaml implementation for A\* CCG parser in:  
[A\* CCG Parsing with a Supertag and Dependency Factored Model](https://arxiv.org/abs/1704.06936).


### Build

#### Requirements

* Python 3
* OCaml (newer version)
* opam (opam-installer)

```sh
$ pip install chainer protobuf spacy
$ opam pin add depccg -k git https://github.com/masashi-y/depccg.ml.git
```

### Usage
```sh
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
|Camelthorn| 88.9% | 94.1%|

```

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

