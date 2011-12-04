# Makefile

CC=gcc
CFLAGS= -c -W -Wall -Werror -std=c99 -pedantic -D_XOPEN_SOURCE=500 -I`ocamlc -where`

OPT=ocamlopt.opt
OCLIBS= graphics.cmxa bigarray.cmxa

COBJ= perlin.o bmp.o stub.o
OCOBJ= perlin_stub.cmx

all: genperlin

genperlin: ${COBJ} ${OCOBJ} genperlin.ml
	${OPT} -o genperlin ${OCLIBS} ${COBJ} ${OCOBJ} genperlin.ml

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OPT) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OPT) $(OCAMLOPTFLAGS) -c $<

clean::
	rm -f *~
	rm -f *.cm[iox] *.o
	rm -f genperlin

# GENERATE IMAGES

POVScenes/carte-pov.png: POVScenes/Makefile genperlin
	cd POVScenes && ${MAKE} carte-pov.png

POVScenes/map.png: genperlin POVScenes/Makefile POVScenes/map.pov
	cd POVScenes && ${MAKE} map.png


# END