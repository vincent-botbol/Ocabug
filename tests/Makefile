CC=ocamlc -thread -I +lablgtk2 lablgtk.cma unix.cma threads.cma lablgtksourceview2.cma gtkInit.cmo

all: utils config ui_starter main

ui_starter:ui_starter.ml
	$(CC) utils.cmo socket_config.cmo $< -o $@

run: all
	./ui_starter

clean::
	rm -fr *.cm[io] *~ ui_starter

config:
	$(CC) -c socket_config.ml

main:
	$(CC) utils.cmo socket_config.cmo ui_starter.cmo oca_main.ml -o main

clean::
	rm -f main

utils:
	$(CC) -c utils.ml