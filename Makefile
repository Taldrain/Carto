.SUFFIXES: .ml .cmx

CC=ocamlopt
RM=rm -f
OUT=carto
ML=refe.ml pre.ml browser.ml assist.ml main.ml
CMX=${ML:.ml=.cmx}
WALL=-I +lablgtk2 -I +sdl -I +lablGL \
lablgl.cmxa \
lablgtk.cmxa \
bigarray.cmxa \
sdl.cmxa \
sdlloader.cmxa
IMG=img/car*

all: ${CMX}
	${CC} ${WALL} ${CMX} -o ${OUT} 

.ml.cmx:
	${CC} ${WALL} -c $<

cleanimg:
	${RM} ${IMG}
cleanall:
	${RM} *.cm* *.o .*.swp ~* '#'* ${IMG} $(OUT)

#END
