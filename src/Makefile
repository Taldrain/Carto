.SUFFIXES: .ml .cmx

CC=ocamlopt.opt
RM=rm -f
OUT=carto
ML=refe.ml parser_obj.ml filter.ml pre.ml post.ml\
   graphics_engine.ml browser.ml assist.ml main.ml
CMX=${ML:.ml=.cmx}
WALL=-thread -I +lablgtk2 -I +sdl -I +lablGL -ccopt -L.\
	 unix.cmxa \
	 threads.cmxa \
	 str.cmxa \
     lablgl.cmxa \
     lablgtk.cmxa \
     bigarray.cmxa \
     sdl.cmxa \
     sdlloader.cmxa \
	 lablglut.cmxa

all: assemble genperlin post

32: lib32 assemble post

ocamlbuild:
	rm -f *.cm* *.so *.a *.o
	ocamlbuild -use-ocamlfind -X generator main.native
	mv main.native carto.native

.ml.cmx:
	${CC} ${WALL} -c $<

lib32:
	ln -fs /usr/lib/libglut.so.3 libglut.so
lib64:
	ln -fs /usr/lib64/libglut.so.3 libglut.so

assemble: ${CMX}
	${CC} ${WALL} ${CMX} -o ${OUT}

genperlin:
	cd generator/ && ${MAKE} &&	mv genperlin ../../

post:
	cp ${OUT} .. && ${RM} ${OUT}

clean:
	${RM} -rf *.cm* *.o .*.swp ~* '#'* *.so *.mli _build/

#END
