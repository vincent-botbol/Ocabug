open Ocabug_view
open Source_viewer

let ebox_list =
    ref ([] :
	    (int * (bool ref * int ref * GMisc.image * Instruct.debug_event))
	    list)

let add_ebox n img ev =
  ebox_list := (n, (ref true, ref (-1), img, ev)) :: !ebox_list

let ebox_from_event event =
  List.find (fun (_,(_,_,_,ev)) -> ev = event) !ebox_list

let ebox_from_break_number break_number =
  List.find (fun (_,(_,bn,_,_)) -> break_number = !bn) !ebox_list

let set_break click img bn new_bn =
  assert !click;
  img#set_pixbuf breakpoint_pixbuf;
  bn := new_bn;
  click := false

let set_break_from_event event break_number =
  let (_,(click,bn,img,_)) = ebox_from_event event in
  set_break click img bn break_number

let set_break_from_ebox_number ebox_number break_number =
  let (click,bn,img,_) = List.assoc ebox_number !ebox_list in
  set_break click img bn break_number

    (* bn is the reference containing the associated breakpoint number with the event *)
let remove_break click bn img =
  assert (!click = false);
  img#set_pixbuf event_pixbuf;
  bn := -1;
  click := true

let remove_break_from_number break_number =
  let (_,(click,bn,img,_)) = ebox_from_break_number break_number in
  remove_break click bn img

let remove_break_from_ebox_number ebox_number =
  let (click,bn,img,_) = List.assoc ebox_number !ebox_list in
  remove_break click bn img

let set_or_remove_break ebox_number callback1 callback2 =
  let (click,bn,_,_) = List.assoc ebox_number !ebox_list in
  if !click then
    (* new_breakpoint *)
    callback1 ()
  else
    (* remove_breakpoint *)
    callback2 !bn ()

