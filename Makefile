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

all: lib assemble

lib:
	ln -s /usr/lib/libglut.so.3 libglut.so

assemble: ${CMX}
	${CC} ${WALL} ${CMX} -o ${OUT}

.ml.cmx:
	${CC} ${WALL} -c $<

cleanall: clean
	${RM} ${OUT} InfoCarto.txt out.bmp
cleanimg:
	${RM} ${IMG}
clean:
	${RM} *.cm* *.o .*.swp ~* '#'* ${IMG} libglut.so

#END
