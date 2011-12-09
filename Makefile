.SUFFIXES: .ml .cmx

VPATH=src/
GPATH=generator/
OUT=supermap genperlin
IMG=img/car*

all:
	cd ${VPATH} && ${MAKE}

32: assemble32

assemble32:
	cd ${VPATH} && ${MAKE} 32

genperlin:
	cd ${VPATH} && ${MAKE} genperlin

ocamlbuild: genperlin
	cd ${VPATH} && ${MAKE} ocamlbuild

clean:
	rm -f ${IMG} && cd ${VPATH} && ${MAKE} clean && cd ${GPATH} && ${MAKE} clean

cleanall: clean
	${RM} ${OUT} InfoCarto.txt supermap.obj
	${RM} out.bmp rand_map.bmp contour?.bmp tmp.bmp median.bmp


#END
