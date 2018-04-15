#!/bin/sh


model_dir=models

if [ ! -d ${model_dir} ]; then
    mkdir -p ${model_dir}
fi

wget -N https://cl.naist.jp/\~masashi-y/resources/depccg/ja_hf_ccgbank.tar.gz -P ${model_dir}
tar xvf ${model_dir}/ja_hf_ccgbank.tar.gz -C ${model_dir}

echo "done"

