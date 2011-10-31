.SUFFIXES: .ml .cmx

VPATH=src/
OUT=carto
IMG=img/car*

all: assemble

64: assemble64

assemble:
	cd ${VPATH} && ${MAKE}

assemble64:
	cd ${VPATH} && ${MAKE} 64

clean:
	rm -f ${IMG} && cd ${VPATH} && ${MAKE} clean

cleanall: clean
	${RM} ${OUT} InfoCarto.txt out.bmp supermap.obj

#END
