#!/bin/sh


model_dir=models

if [ ! -d ${model_dir} ]; then
    mkdir -p ${model_dir}
fi

wget -N https://cl.naist.jp/\~masashi-y/resources/depccg/en_hf_tri.tar.gz -P ${model_dir}
tar xvf ${model_dir}/en_hf_tri.tar.gz -C ${model_dir}

echo "done"

