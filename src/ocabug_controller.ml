open Ocabug_view
open Ocabug_misc
open Debugger_config

(**************************
	    ICONS
**************************)



module Icons_controller =
struct

  let run_callback () = 
    Printf.printf "run pressed\n%!"

  let backstep_callback () = 
    Printf.printf "backstep pressed\n%!"

  let step_callback () = 
    Printf.printf "step pressed\n%!"

  let icons_callbacks =
    [ ("../img/icons/backstep.png", backstep_callback)
    ; ("../img/icons/run.png", run_callback)
    ; ("../img/icons/step.png", step_callback) ]

  let key_callback cmd entry key =
    let value = GdkEvent.Key.keyval key in
    if value = 65421 || value = 65293 then
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
      end;
    false

	

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
	      ~buffer:(GText.buffer ~text:"Printer's path name (e.g : Printers.my_printer)" () )
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

  let load_source () = Source_viewer.load_source_file (!Parameters.program_name ^ ".ml")

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
    Printf.printf "[DEBUG] Msg sent\n";
    begin
      try
	ignore (Command_line.interprete_line Format.std_formatter entry#text);
      with
	| Command_line.Command_line -> Printf.printf "command line exn caught\n%!"
	| Debugger_config.Toplevel -> Printf.printf "Toplevel exn caught\n%!"
    end;
    (* to know when to stop reading *)
    let answer = Ocabug_misc.force_read () in
    if answer <> "" then
      write_buffer (answer^"\n");
    entry#set_text "";
    (* TODO : Parse de l'event pour se positionner dans le source *)
    
    (* Suivi automatique du scrolling *)
    let adj = sw#vadjustment in
    adj#set_value (adj#upper -. adj#page_size)



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
  Icons_controller.add_icons ();
  Source_controller.load_source ();
  Modules_controller.load_modules ();
  Toplevel_controller.connect ();
  print_endline "Everything loaded";
  window#show ();
  GMain.Main.main ()
  
