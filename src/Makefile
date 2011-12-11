.SUFFIXES: .ml .cmx

CC=ocamlopt.opt
RM=rm -f
OUT=supermap
ML=refe.ml parser_obj.ml filter.ml pre.ml post.ml\
   camera.ml graphics_engine.ml browser.ml assist.ml main.ml argv.ml
CMX=${ML:.ml=.cmx}
WALL=-I +lablgtk2 -I +sdl -I +lablGL -ccopt -L.\
	 str.cmxa       \
     lablgl.cmxa    \
     lablgtk.cmxa   \
     bigarray.cmxa  \
     sdl.cmxa       \
     sdlloader.cmxa \
	 lablglut.cmxa  \
	 lablgtkgl.cmxa

all: assemble genperlin post

32: assemble genperlin post

ocamlbuild:
	rm -f *.cm* *.so *.a *.o
	ocamlbuild -use-ocamlfind -X generator main.native
	mv main.native carto.native

.ml.cmx:
	${CC} ${WALL} -c $<

assemble: ${CMX}
	${CC} ${WALL} ${CMX} -o ${OUT}

genperlin:
	cd generator/ && ${MAKE} &&	mv genperlin ../../

post:
	cp ${OUT} .. && ${RM} ${OUT}

clean:
	${RM} -rf *.cm* *.o .*.swp ~* '#'* *.so *.mli _build/

#END
