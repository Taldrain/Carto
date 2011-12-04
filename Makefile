.SUFFIXES: .ml .cmx

VPATH=src/
GPATH=generator/
OUT=carto genperlin
IMG=img/car*

all:
	cd ${VPATH} && ${MAKE}

ocamlbuild:
	cd ${VPATH} && ${MAKE} ocamlbuild

mli:
	cd ${VPATH} && ${MAKE} mli

32: assemble32

assemble32:
	cd ${VPATH} && ${MAKE} 32

assemble64:
	cd ${VPATH} && ${MAKE}

mli:
	cd ${VPATH} && ${MAKE} mli

clean:
	rm -f ${IMG} && cd ${VPATH} && ${MAKE} clean && cd ${GPATH} && ${MAKE} clean

cleanall: clean
	${RM} ${OUT} InfoCarto.txt out.bmp supermap.obj

#END
