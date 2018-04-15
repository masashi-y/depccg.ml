PREFIX=/usr/local
LIBDIR=$(PREFIX)/lib/camelthorn
SHAREDIR=$(PREFIX)/share/camelthorn
MODELDIR=models


install: lib depccgrc
	install -d $(SHAREDIR)
	install -d $(SHAREDIR)/tri_headfirst
	install -d $(SHAREDIR)/ja_headfinal
	install -m 644 models/tri_headfirst/* $(SHAREDIR)/tri_headfirst
	install -m 644 models/ja_headfinal/* $(SHAREDIR)/ja_headfinal
	install -m 644 depccgrc $(HOME)/.depccgrc


lib: depccgrc
	# if [ ! -d ${MODELDIR} ]; then \
	# fi
	mkdir -p ${MODELDIR}
	wget https://cl.naist.jp/\~masashi-y/resources/depccg/en_hf_tri.tar.gz -P ${MODELDIR}
	tar xvf ${MODELDIR}/en_hf_tri.tar.gz -C ${MODELDIR}
	wget https://cl.naist.jp/\~masashi-y/resources/depccg/ja_hf_ccgbank.tar.gz -P ${MODELDIR}
	tar xvf ${MODELDIR}/ja_hf_ccgbank.tar.gz -C ${MODELDIR}
	echo "done"
	# sh ./download_en_model.sh
	# sh ./download_ja_model.sh

depccgrc:
	echo "(en ("                               > depccgrc
	echo " (lib ${LIBDIR})"                    >> depccgrc
	echo " (share ${SHAREDIR})"                >> depccgrc
	echo " (model ${SHAREDIR}/tri_headfirst/)" >> depccgrc
	echo "))"                                  >> depccgrc
	echo "(ja ("                               >> depccgrc
	echo " (lib ${LIBDIR})"                    >> depccgrc
	echo " (share ${SHAREDIR})"                >> depccgrc
	echo " (model ${SHAREDIR}/ja_headfinal)"   >> depccgrc
	echo "))"                                  >> depccgrc
