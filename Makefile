PREFIX=/usr/local
LIBDIR=$(PREFIX)/lib/depccg
SHAREDIR=$(PREFIX)/share/depccg
MODELDIR=models

install: lib depccgrc
	install -d $(SHAREDIR)
	install -d $(SHAREDIR)/tri_headfirst
	install -d $(SHAREDIR)/ja_headfinal
	install -m 644 models/tri_headfirst/* $(SHAREDIR)/tri_headfirst
	install -m 644 models/ja_headfinal/* $(SHAREDIR)/ja_headfinal
	install -m 644 depccgrc $(HOME)/.depccgrc


${MODELDIR}:
	sh ./download_en_model.sh
	sh ./download_ja_model.sh

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
