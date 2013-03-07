CC=ocamlc
FLAGS=-I +lablgtk2 lablgtk.cma lablgtksourceview2.cma gtkInit.cmo

all: test

test:test.ml
	$(CC) $(FLAGS) -o $@ $<

run: all
	./test

clean:
	rm -fr *.cm[io] *~ test