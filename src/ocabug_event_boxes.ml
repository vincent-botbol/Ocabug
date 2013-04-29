open Ocabug_view
open Ocabug_misc
open Source_viewer
open Instruct
open Hashtbl

exception No_such_event_box

type event_box =
    { mutable click : bool; (* to know if we want to create or
			       remove a breakpoint *)
      mutable highlighted : bool; (* true if current event *)
      mutable break_number : int;
      mutable counter : int; (* how many times we go through this event *)
      mutable image : GMisc.image;
      event : Instruct.debug_event
    }

let event_boxes_by_module :
    (string, (int * event_box) list) Hashtbl.t
     = Hashtbl.create 10
let current_event_boxes : (int * event_box) list ref
    = ref []


let ebox ev img_pixbuf =
  { click = true;
    highlighted = false;
    break_number = -1;
    counter = 0;
    image = GMisc.image ~pixbuf:img_pixbuf ();
    event = ev
  }
    
let add_eboxes mdl events img_pixbuf=
  Hashtbl.add event_boxes_by_module mdl
    (list_mapi (fun n ev -> (n, ebox ev img_pixbuf)) events)
    
    
let ebox_from_event ev =
  try
    snd
      (List.find (fun (_,ebox) -> ebox.event.ev_pos = ev.ev_pos)
	 (Hashtbl.find event_boxes_by_module ev.ev_module))
  with
    | Not_found -> raise No_such_event_box
      
let ebox_from_break_number break_number =
  let res = ref None in
  try
    Hashtbl.iter
      (fun _ ebox_list ->
	try
	  res := Some (snd (List.find (fun (_,ebox) ->
	    ebox.break_number = break_number) ebox_list));
	  raise Exit
	with
	  | Not_found -> ()
      )
      event_boxes_by_module;
    failwith "ebox not found"
  with
    | Exit -> match !res with Some ebox -> ebox | None -> failwith "impossible"
      
      
	    
let set_break ebox new_bn =
  assert ebox.click;
  if ebox.highlighted then
    ebox.image#set_pixbuf breakpoint_pixbuf2
  else
    ebox.image#set_pixbuf breakpoint_pixbuf;
  ebox.break_number <- new_bn;
  ebox.click <- false

let set_break_from_event event break_number =
  set_break (ebox_from_event event) break_number

let set_break_from_ebox_number ebox_number break_number =
  try
    set_break (List.assoc ebox_number !current_event_boxes) break_number
  with
      Not_found -> raise No_such_event_box

    (* bn is the reference containing the associated breakpoint number with the event *)
let remove_break ebox =
  assert (not ebox.click);
  if ebox.highlighted then
    ebox.image#set_pixbuf event_pixbuf2
  else
    ebox.image#set_pixbuf event_pixbuf;
  ebox.break_number <- -1;
  ebox.click <- true

let remove_break_from_number break_number =
  remove_break (ebox_from_break_number break_number)

let remove_break_from_ebox_number ebox_number =
  try
    remove_break (List.assoc ebox_number !current_event_boxes)
  with
      Not_found -> raise No_such_event_box

let set_or_remove_break ebox_number callback1 callback2 =
  let {click = b; break_number = bn} =
    (List.assoc ebox_number !current_event_boxes)
  in
  if b then
    (* new_breakpoint *)
    callback1 ()
  else
    (* remove_breakpoint *)
    callback2 bn ()

let highlight ev =
  try
    let ebox = ebox_from_event ev in
    ebox.highlighted <- true;
    if ebox.click then
      ebox.image#set_pixbuf event_pixbuf2
    else
      ebox.image#set_pixbuf breakpoint_pixbuf2
  with
    | No_such_event_box -> ()

let remove_highlight ev =
  try
    let ebox = ebox_from_event ev in
    ebox.highlighted <- false;
    if ebox.click then
      ebox.image#set_pixbuf event_pixbuf
    else
      ebox.image#set_pixbuf breakpoint_pixbuf
  with
    | No_such_event_box -> ()


