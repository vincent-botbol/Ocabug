CC=ocamlc -I +lablgtk2 lablgtk.cma lablgtksourceview2.cma

all: ui_starter

ui_starter:ui_starter.ml
	$(CC) $< -o $@

run: all
	./ui_starter

clean:
	rm -fr *.cm[io] *~ ui_starter