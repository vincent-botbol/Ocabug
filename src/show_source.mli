(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: show_source.mli 7767 2006-12-09 13:49:10Z ertai $ *)

open Format

(* Print the line containing the point *)
val show_point : formatter -> Instruct.debug_event -> bool -> unit;;

(* Tell Emacs we are nowhere in the source. *)
val show_no_point : formatter -> unit;;

(* Display part of the source. *)
val show_listing : formatter -> Lexing.position -> string -> int -> int -> int -> bool -> unit;;
