Ocabug
======

TODO:
- Améliorer l'interface graphique
- Mettre en surbrillance l'event courant
- Compter le nombre de passage sur un event
  (pour cela, il faut toucher à internal_step dans time_travel.ml et ne faire des step que de 1 mais n fois plutôt qu'un step de n)
- Détecter le changement de thread dans la VM
- ...

Visual ocaml debugger

Pour compiler, éditez le src/Makefile et indiquez le chemin vers les sources d'ocaml. Il faut également compiler les sources OCaml avant de pouvoir compiler le déboggueur visuel.

packages required :

liblablgtk2-ocaml[-dev]
liblablgtksourceview2-ocaml[-dev]
