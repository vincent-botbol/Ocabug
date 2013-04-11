open Ocabug_view
open Ocabug_misc
open Debugger_config

(**************************
	    ICONS
**************************)



module Icons_controller =
struct

  let cmd_callback cmd () =
    ignore (Command_line.interprete_line Ocabug_config.formatter cmd);
    Command_invite.adjust_window ()

  let icons_callbacks =
    [ ("../img/icons/backstep.png", cmd_callback "backstep")
    ; ("../img/icons/run.png", cmd_callback "run")
    ; ("../img/icons/step.png", cmd_callback "step") 
    ; ("../img/icons/temp/bullet_left.png", cmd_callback "reverse") ]

  let key_callback cmd entry key =
    let value = GdkEvent.Key.keyval key in
    if value = 65421 || value = 65293 then
      begin
	cmd_callback (cmd^" "^entry#text) ();
	entry#set_text "";
	true
      end
    else
      false
(*
      begin
	try
	  ignore (Command_line.interprete_line Ocabug_config.formatter (cmd^" "^entry#text));
	  entry#set_text "";
	  let answer = force_read () in
	  if answer <> "" then
	    Command_invite.write_buffer answer;
	with
	  | Toplevel ->
	    Printf.fprintf stdout "Toplevel exn caught\n%!";
	    entry#set_text "";
	    write (force_read ())
	  | Command_line.Command_line ->
	    Printf.fprintf stdout "Command_line exn caught\n%!";
	    entry#set_text "";
	    write (force_read ())
      end;*)

	

  let user_printer_callback () =
    let w = GWindow.window () in
    let packer = GPack.vbox ~packing:w#add () in
    ignore (GText.view
	      ~packing:packer#add
	      ~editable:false
	      ~cursor_visible:false
	      ~buffer:(GText.buffer ~text:"Load printer object file (e.g : printers.cmo)" () )
	      ()
    );
    let e1 = GEdit.entry ~text:"" ~packing:packer#add ~editable:true () in
    ignore (e1#event#connect#key_press ~callback:(key_callback "load_printer" e1));
    ignore (GText.view
	      ~packing:packer#add
	      ~editable:false
	      ~cursor_visible:false
	      ~buffer:(GText.buffer
			 ~text:
			 "Printer's path name (e.g : Printers.my_printer)\n
Type must be : outchan -> t -> unit"
			 () )
	      ()
    );
    let e2 = GEdit.entry ~text:"" ~packing:packer#add ~editable:true () in
    ignore (e2#event#connect#key_press ~callback:(key_callback "install_printer" e2));
    w#show ()

  let add_icons () =
    Icons_viewer.link_icons icons_callbacks;
    ignore (Icons_viewer.user_printer_button#connect#clicked user_printer_callback)

end


(***********************************************
	     SOURCE CONTROLLER
***********************************************)

module Source_controller =
struct

  open Source_viewer

  let load_source () = load_source_file (!Parameters.program_name ^ ".ml")


  (* here comes events and breakpoints icons handling *)

    (* to fix event boxes offset *)
  (*let fix_offset = let n =  ref (-1) in (fun () -> incr n; !n)*)


  open Ocabug_event_boxes

  let break_callback ev = fun () -> Breakpoints.new_breakpoint ev
  let event_callback n = fun () -> Breakpoints.remove_breakpoint n

  let event_break_callback n ev img b =
    set_or_remove_break
      (* break number incremented because call to new_event is not done yet *)
      n
      (break_callback ev)
      event_callback;
    true

  let make_event_box n ev =
    let offset = (Events.get_pos ev).Lexing.pos_cnum + n in
    let ebox = GBin.event_box ~show:true () in
    let img = GMisc.image ~pixbuf:event_pixbuf ~packing:ebox#add () in
    (* see above *)
    add_ebox n img ev;
    (* inserts event box in buffer *)
    ignore(
      source_box#add_child_at_anchor 
	(ebox :> GObj.widget) 
	(source_buffer#create_child_anchor (source_buffer#get_iter (`OFFSET offset))));
    ignore(ebox#event#connect#button_press ~callback:(event_break_callback n ev img))

  let load_events ev_module =
    let l = Symbols.events_in_module ev_module in
    list_iteri make_event_box l
    

end




(**********************************************
	      MODULES CONTROLLER
**********************************************)


module Modules_controller =
struct

  let current_mod = ref ""

  let set_module m () =
    if m <> !current_mod then
      begin
	current_mod := m;
	print_endline m
      end

  let load_modules () =
    Program_management.ensure_loaded ();
    let mods = !Symbols.modules in
    if mods <> [] then current_mod := List.hd mods;
    List.iter
      (fun mod_name ->
	let item = Modules_combo.make_arrow_label ~label:mod_name ~string:mod_name in
	ignore (item#connect#select ~callback:(set_module mod_name))
      )
      mods

end




(*************************************************
		TOPLEVEL CONTROLLER
*************************************************)


module Toplevel_controller =
struct

  open Command_invite

  let last_command = ref ""

  let send () =
    ignore (Command_line.interprete_line Format.std_formatter entry#text);
    entry#set_text "";
    (* TODO : Parse de l'event pour se positionner dans le source *)
    
    (* Suivi automatique du scrolling *)
    adjust_window ()



  let key_callback key =
    let value = GdkEvent.Key.keyval key in
    if value = 65421 || value = 65293 then 
      begin send (); false; end
    (* up = 65362 | down = 65364 *)
    else if value = 65362 then
      begin
	entry#set_text !last_command;
	entry#set_position (String.length !last_command);
	true; 
      end
    else if value = 65364 then
      true
    else 
      false

  let connect () =
    ignore (entry#event#connect#key_press ~callback:key_callback);
    ignore (button#connect#clicked ~callback:send)

end








(********************************
	       MAIN
********************************)

let show_ui () =
  print_endline "Start show_ui";
  Program_management.ensure_loaded ();
  Icons_controller.add_icons ();
  Source_controller.load_source ();
  Source_controller.load_events "Test";
  Modules_controller.load_modules ();
  Toplevel_controller.connect ();
  print_endline "Everything loaded";
  window#show ();
  GMain.Main.main ()
  
