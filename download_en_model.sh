#!/bin/sh


model_dir=models

if [ ! -d ${model_dir} ]; then
    mkdir -p ${model_dir}
fi

cd ${model_dir}
wget https://cl.naist.jp/\~masashi-y/resources/depccg/en_hf_tri.tar.gz
tar xvf en_hf_tri.tar.gz

echo "done"

cd ..
