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

(* $Id: show_source.ml 9540 2010-01-20 16:26:46Z doligez $ *)

open Debugger_config
open Instruct
open Parameters
open Primitives
open Format
open Source
open Ocabug_misc

let no_source_found_message mdl =
  "No source file found for module " ^ mdl ^
    ".\nTry \"directory source_directory\"."

(* Print a line; return the beginning of the next line *)
let print_line ppf buffer line_number start point before =
  let next = next_linefeed buffer start
  and content = buffer_content buffer
  in
    fprintf ppf "%i " line_number;
    if point <= next && point >= start then
      (fprintf ppf "%s%!" (String.sub content start (point - start));
       fprintf ppf "%s%!" (if before then event_mark_before else event_mark_after);
       fprintf ppf "%s\n%!" (String.sub content point (next - point)))
    else
      fprintf ppf "%s\n%!" (String.sub content start (next - start));
    next

(* Tell Emacs we are nowhere in the source. *)
let show_no_point ppf =
  if !emacs then fprintf ppf "\026\026H\n"

(* Print the line containing the point *)
let show_point ppf ev selected =
  let mdle = ev.ev_module in
  let before = (ev.ev_kind = Event_before) in
  if !emacs && selected then
    begin
      try
	let buffer = get_buffer (Events.get_pos ev) mdle in
	let source = source_of_module ev.ev_loc.Location.loc_start mdle in
	fprintf ppf "\026\026M%s:%i:%i" source
          (snd (start_and_cnum buffer ev.ev_loc.Location.loc_start))
          (snd (start_and_cnum buffer ev.ev_loc.Location.loc_end));
	fprintf ppf "%s\n" (if before then ":before" else ":after")
      with
	| Out_of_range -> (* point_of_coord *)
          print_error "Position out of range."
	| Not_found    -> (* Events.get_pos || get_buffer *)
	  print_error (no_source_found_message mdle);
          show_no_point ppf
    end
  else
    begin try
      let pos = Events.get_pos ev in
      let buffer = get_buffer pos mdle in
      let start, point = start_and_cnum buffer pos in
      ignore(print_line ppf buffer pos.Lexing.pos_lnum start point before)
    with
      Out_of_range -> (* point_of_coord *)
        print_error "Position out of range."
    | Not_found    -> (* Events.get_pos || get_buffer *)
        print_error (no_source_found_message mdle)
    end

(* Display part of the source. *)
let show_listing ppf pos mdle start stop point before =
  try
    let buffer = get_buffer pos mdle in
      let rec aff (line_start, line_number) =
        if line_number <= stop then
          aff (print_line ppf buffer line_number line_start point before + 1, line_number + 1)
      in
        aff (pos_of_line buffer start)
  with
    Out_of_range -> (* pos_of_line *)
      print_error "Position out of range."
  | Not_found    -> (* get_buffer *)
      print_error (no_source_found_message mdle)
