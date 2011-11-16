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

all: lib64 assemble post

32: lib32 assemble post

lib32:
	ln -fs /usr/lib/libglut.so.3 libglut.so
lib64:
	ln -fs /usr/lib64/libglut.so.3 libglut.so

.ml.cmx:
	${CC} ${WALL} -c $<

assemble: ${CMX}
	${CC} ${WALL} ${CMX} -o ${OUT}

post:
	cp ${OUT} .. && ${RM} ${OUT}

clean:
	${RM} *.cm* *.o .*.swp ~* '#'* libglut.so

cleanall: clean
	${RM} ${OUT} InfoCarto.txt out.bmp supermap.obj

#END
