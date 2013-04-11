open Ocabug_view
open Source_viewer
open Instruct
open Hashtbl

exception No_such_event_box

type event_box =
    { mutable click : bool; (* to know if we want to create or
			       remove a breakpoint *)
      mutable break_number : int;
      mutable counter : int; (* how many times we go through this event *)
      image : GMisc.image;
      event : Instruct.debug_event
    }

let event_boxes : (int, event_box) Hashtbl.t = Hashtbl.create 30

let ebox_list :
    (int * (bool ref * int ref * GMisc.image * Instruct.debug_event)) list ref
    = ref []

let add_ebox n img ev =
  add event_boxes n
    { click = true;
      break_number = -1;
      counter = 0;
      image = img;
      event = ev
    }

let ebox_from_event ev =
  let res = ref None in
  try
    Hashtbl.iter (fun _ ebox ->
      if ebox.event.ev_pos = ev.ev_pos then
	begin res := Some ebox; raise Exit end)
      event_boxes;
    raise No_such_event_box
  with
    | Exit -> (match !res with Some x -> x | _ -> raise No_such_event_box)

let ebox_from_break_number break_number =
  let res = ref None in
  try
    Hashtbl.iter (fun _ ebox ->
      if ebox.break_number = break_number then
	begin res := Some ebox; raise Exit end)
      event_boxes;
    raise No_such_event_box
  with
    | Exit -> (match !res with Some x -> x | _ -> raise No_such_event_box)

let set_break ebox new_bn =
  assert ebox.click;
  ebox.image#set_pixbuf breakpoint_pixbuf;
  ebox.break_number <- new_bn;
  ebox.click <- false

let set_break_from_event event break_number =
  set_break (ebox_from_event event) break_number

let set_break_from_ebox_number ebox_number break_number =
  try
    set_break (Hashtbl.find event_boxes ebox_number) break_number
  with
      Not_found -> raise No_such_event_box

    (* bn is the reference containing the associated breakpoint number with the event *)
let remove_break ebox =
  assert (not ebox.click);
  ebox.image#set_pixbuf event_pixbuf;
  ebox.break_number <- -1;
  ebox.click <- true

let remove_break_from_number break_number =
  remove_break (ebox_from_break_number break_number)

let remove_break_from_ebox_number ebox_number =
  try
    remove_break (Hashtbl.find event_boxes ebox_number)
  with
      Not_found -> raise No_such_event_box
	
let set_or_remove_break ebox_number callback1 callback2 =
  let {click = b; break_number = bn} =
    (Hashtbl.find event_boxes ebox_number)
  in
  if b then
    (* new_breakpoint *)
    callback1 ()
  else
    (* remove_breakpoint *)
    callback2 bn ()

