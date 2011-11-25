.SUFFIXES: .ml .cmx

CC=ocamlopt
RM=rm -f
OUT=carto
ML=refe.ml parser_obj.ml pre.ml post.ml graphics_engine.ml browser.ml assist.ml main.ml
CMX=${ML:.ml=.cmx}
WALL=-I +lablgtk2 -I +sdl -I +lablGL -ccopt -L.\
	str.cmxa \
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
	ln -fs /usr/lib/libgtkgl-2.0.so.1 libgtkgl-2.0.so
lib64:
	ln -fs /usr/lib64/libglut.so.3 libglut.so
	ln -fs /usr/lib64/libgtkgl-2.0.so.1 libgtkgl-2.0.so

.ml.cmx:
	${CC} ${WALL} -c $<

assemble: ${CMX}
	${CC} ${WALL} ${CMX} -o ${OUT}

post:
	cp ${OUT} .. && ${RM} ${OUT}

clean:
	${RM} *.cm* *.o .*.swp ~* '#'* *.so

cleanall: clean
	${RM} ${OUT} InfoCarto.txt out.bmp supermap.obj

#END
