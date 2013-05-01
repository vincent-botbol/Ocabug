Ocabug
======

TODO:
- Améliorer l'interface graphique : Toplevel (+ les délires de layout sur resize)
- Ajouter un bouton "Bigstep"
- Compter le nombre de passage sur un event
  (pour cela, il faut toucher à internal_step dans time_travel.ml et ne faire des step que de 1 mais n fois plutôt qu'un step de n)
- Détecter le changement de thread dans la VM
- Gérer les images pour garder la cohérence entre évènements parents
- ...

Visual ocaml debugger

Pour compiler, éditez le src/Makefile et indiquez le chemin vers les sources d'ocaml. Il faut également compiler les sources OCaml avant de pouvoir compiler le déboggueur visuel.

packages required :

liblablgtk2-ocaml[-dev]
liblablgtksourceview2-ocaml[-dev]

Found bugs :

my_protect => sig_int : rappelle show_ui ~~
Exit => plus propre. Peut-être gérer au niveau du controller plutôt que de la view (Fatal error : EOF) -> fermer les io
toolbar => le premier élément prend le focus, j'ai aucune idée du pourquoi
	   solution possible : donner le focus au toplevel