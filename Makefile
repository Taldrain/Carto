.SUFFIXES: .ml .cmx

VPATH=src/
GPATH=generator/
OUT=carto genperlin
IMG=img/car*

all:
	cd ${VPATH} && ${MAKE}

genperlin:
	cd ${VPATH} && ${MAKE} genperlin

ocamlbuild: genperlin
	cd ${VPATH} && ${MAKE} ocamlbuild

32: assemble32

assemble32:
	cd ${VPATH} && ${MAKE} 32

clean:
	rm -f ${IMG} && cd ${VPATH} && ${MAKE} clean && cd ${GPATH} && ${MAKE} clean

cleanall: clean
	${RM} ${OUT} InfoCarto.txt out.bmp supermap.obj
	             rand_map.bmp contour?.bmp tmp.bmp median.bmp

#END
