#!/bin/sh


model_dir=models

if [ ! -d ${model_dir} ]; then
    mkdir -p ${model_dir}
fi

cd ${model_dir}
wget https://cl.naist.jp/\~masashi-y/resources/depccg/ja_hf_ccgbank.tar.gz
tar xvf ja_hf_ccgbank.tar.gz

echo "done"

cd ..
