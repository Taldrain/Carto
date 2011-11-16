.SUFFIXES: .ml .cmx

VPATH=src/
OUT=carto
IMG=img/car*

all: assemble64

32: assemble32

assemble32:
	cd ${VPATH} && ${MAKE}

assemble64:
	cd ${VPATH} && ${MAKE} 32

clean:
	rm -f ${IMG} && cd ${VPATH} && ${MAKE} clean

cleanall: clean
	${RM} ${OUT} InfoCarto.txt out.bmp supermap.obj

#END
