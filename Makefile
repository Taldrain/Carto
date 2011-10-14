CC=ocamlopt
RM=rm -rf
OUT=carto
FILE=refe.ml pre.ml browser.ml assist.ml main.ml
FILEX=refe.cmx pre.cmx browser.cmx assist.cmx main.cmx
WALL=-I +lablgtk2 -I +sdl -I +lablGL lablgl.cmxa lablgtk.cmxa bigarray.cmxa sdl.cmxa sdlloader.cmxa

all: $(FILEX) 
	$(CC) $(WALL) $^ -o $(OUT) 

$(FILEX): $(FILE)
	$(CC) $(WALL) -c $^

clean:
	$(RM) *.cm* *.o .*.swp ~* '#'* $(OUT)
