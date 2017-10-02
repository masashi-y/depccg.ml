# Camelthorn :seedling::deciduous_tree::camel:

A* CCG parser implemented in OCaml

### Build
```sh
$ opam install psq ocaml-protoc
$ git clone git://github.com/masashi-y/Camelthorn.git
$ cd Camelthorn
$ omake
```

### Usage
```sh
$ echo "this is a test example." > input.txt
$ ./run.sh input.txt
```

| Parser | labeled F1 | unlabeled F1 |
|:------:|:----------:|:------------:|
|depccg  | 88.8% | 94.0% |
|Camelthorn| 88.9% | 94.1%|
