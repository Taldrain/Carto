.SUFFIXES: .ml .mli .cmx .cmi

CC=ocamlopt.opt
RM=rm -f
OUT=supermap
ML=refe.ml parser_obj.ml filter.ml pre.ml post.ml\
   graphics_engine.ml browser.ml assist.ml main.ml argv.ml
CMX=${ML:.ml=.cmx}
MLI=${ML:.ml=.mli}
CMI=${ML:.ml=.cmi}
WALL=-I +lablgtk2 -I +sdl -I +lablGL -ccopt -L.\
	 str.cmxa       \
     lablgl.cmxa    \
     lablgtk.cmxa   \
     bigarray.cmxa  \
     sdl.cmxa       \
     sdlloader.cmxa \
	 lablglut.cmxa  \
	 lablgtkgl.cmxa

all: link assemble genperlin post cleanso

32: link32 assemble genperlin post cleanso

ocamlbuild:
	rm -f *.cm* *.so *.a *.o
	ocamlbuild -use-ocamlfind -X generator main.native
	mv main.native carto.native

link:
	ln -fs /usr/lib64/libglut.so.3 libglut.so
	ln -fs /usr/lib64/libgtkgl-2.0.so.1 libgtkgl-2.0.so

link32:
	ln -fs /usr/lib/libglut.so.3 libglut.so
	ln -fs /usr/lib/libgtkgl-2.0.so.1 libgtkgl-2.0.so

cleanso:
	${RM} *.so

.ml.cmx:
	${CC} ${WALL} -c $<

.mli.cmi:
	${CC} ${WALL} -c $<

assemble: ${CMI} ${CMX}
	${CC} ${WALL} ${CMX} -o ${OUT}

genperlin:
	cd generator/ && ${MAKE} &&	mv genperlin ../../

post:
	cp ${OUT} .. && ${RM} ${OUT}

clean:
	${RM} -rf *.cm* *.o .*.swp ~* '#'* *.so  _build/

#END
