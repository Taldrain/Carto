CC=ocamlopt
RM=rm -rf
OUT=carto
FILE=refe.ml pre.ml browser.ml main.ml

all: ${FILE} 
	${CC} -I +lablgtk2 -I +sdl -I +lablGL -o ${OUT} lablgl.cmxa lablgtk.cmxa bigarray.cmxa sdl.cmxa sdlloader.cmxa ${FILE}

simple: simple.ml
	${CC} -I +lablgtk2 -I +lablGL -o ${OUT} lablgtk.cmxa lablgl.cmxa lablgtkgl.cmxa simple.ml

clean::
	${RM} *.cm* *.o
