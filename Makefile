.SUFFIXES: .ml .cmx

CC=ocamlopt
RM=rm -f
OUT=carto
ML=refe.ml pre.ml post.ml graphics_engine.ml browser.ml assist.ml main.ml
CMX=${ML:.ml=.cmx}
WALL=-I +lablgtk2 -I +sdl -I +lablGL -ccopt -L.\
lablgl.cmxa \
lablgtk.cmxa \
bigarray.cmxa \
sdl.cmxa \
sdlloader.cmxa \
lablglut.cmxa
IMG=img/car*

all: lib assemble cleanso

64: lib64 assemble cleanso

lib64:
	ln -s /usr/lib64/libglut.so.3 libglut.so

lib:
	ln -s /usr/lib/libglut.so.3 libglut.so

cleanso:
	${RM} libglut.so

assemble: ${CMX}
	${CC} ${WALL} ${CMX} -o ${OUT}

.ml.cmx:
	${CC} ${WALL} -c $<

cleanall: clean
	${RM} ${OUT} InfoCarto.txt out.bmp supermap.obj
cleanimg:
	${RM} ${IMG}
clean: cleanso
	${RM} *.cm* *.o .*.swp ~* '#'* ${IMG}

#END
