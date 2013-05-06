open Ocabug_view
open Ocabug_misc
open Debugger_config

(**************************
	    BUTTONS
**************************)



module Buttons_controller =
struct

  let skip_library = ref true
  let step_callback_id = ref None

  let cmd_callback cmd () =
    ignore (Command_line.interprete_line Ocabug_config.formatter cmd);
    Command_invite.adjust_window ()

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
	
  let user_printer_callback () =
    let w = GWindow.window () in
    let packer = GPack.vbox ~packing:w#add () in
    ignore (GMisc.label ~text:"Load printer object file (e.g : printers.cmo)"
	      ~packing:packer#add ());
    let e1 = GEdit.entry ~text:"" ~packing:packer#add ~editable:true () in
    ignore (e1#event#connect#key_press ~callback:(key_callback "load_printer" e1));
    ignore (GMisc.label ~text:"Printer's path name (e.g : Printers.my_printer)
Type must be : outchan -> t -> unit"
	      ~packing:packer#add ());
    let e2 = GEdit.entry ~text:"" ~packing:packer#add ~editable:true () in
    ignore (e2#event#connect#key_press ~callback:(key_callback "install_printer" e2));
    w#show ()

  let set_icons_callbacks () =
    List.iter (function 
    | (Buttons_viewer.Step, b) -> step_callback_id := 
      Some(b#connect#clicked ~callback:(cmd_callback (if !skip_library then "ocabigstep" else "step")))
    | (Buttons_viewer.Backstep, b) -> ignore(b#connect#clicked ~callback:(cmd_callback  "backstep"))
    | (Buttons_viewer.Run, b) -> ignore(b#connect#clicked ~callback:(cmd_callback  "run"))
    | (Buttons_viewer.Reverse, b) -> ignore(b#connect#clicked ~callback:(cmd_callback  "reverse"))
    | (Buttons_viewer.Bigstep, b) -> ignore(b#connect#clicked ~callback:(cmd_callback  "ocastep"))
    )
      Buttons_viewer.buttons

  let switch_step_command_callback () =
    try 
      let (_, b) = List.find (function (Buttons_viewer.Step, _) -> true | _ -> false) Buttons_viewer.buttons in
      match !step_callback_id with
      | Some id -> 
	b#misc#disconnect id;
	skip_library := not !skip_library;
	step_callback_id := Some (b#connect#clicked 
	  ~callback:(cmd_callback (if !skip_library then "ocabigstep" else "step")))
      | None -> ()
    with
      Not_found -> assert false
       let create_mod_list list labelframe packer =
    let cols = new GTree.column_list in
    let mod_col = cols#add Gobject.Data.string in
    let store = GTree.list_store cols in
    let tree_view = 
      let frame =
	GBin.frame ~border_width:5
	  ~label:labelframe
	  ~shadow_type:`IN
	  ~packing:packer#add () in
      let sw = GBin.scrolled_window ~packing:frame#add () in
      GTree.view ~model:store ~reorderable:true
	~packing:sw#add () in
    ignore(
      tree_view#append_column
	(GTree.view_column ~title:"Module"
	   ~renderer:(GTree.cell_renderer_text [], [ "text", mod_col ]) ()));
    List.iter (fun x ->
      let row = store#append () in
      store#set ~row ~column:mod_col x
    ) list;
    (tree_view, store)
      
  let activate_module (eview, excl_store) 
      (aview, acti_store) () =
    if eview#selection#count_selected_rows == 0 then
      ()
    else
      List.iter 
	(fun item -> 
	  ignore item
	)
	eview#selection#get_selected_rows
      
      
  let display_filter_list () =
    let w = GWindow.window ~height:400 () in
    let packer = GPack.hbox ~packing:w#add () in
    let (eview, excl_store) as excl = create_mod_list (!Symbols.exclude_modules) "Filtered modules" packer in
    let vbox = GPack.vbox ~packing:packer#add () in
    let (aview, acti_store) as acti = create_mod_list (!Symbols.active_modules) "Active Modules" packer in
    let b1 = GButton.button ~label:"Activate ->" ~packing:vbox#add () in
    let b2 = GButton.button ~label:"<- Exclude" ~packing:vbox#add () in
    ignore (b1#connect#clicked (activate_module excl acti));
    ignore b2;
    w#show ()

  let set_menu_callbacks () =
    ignore(Menu_viewer.tools_menu_printer#connect#activate user_printer_callback);
    ignore(Menu_viewer.tools_menu_step_choice#connect#toggled switch_step_command_callback);
    ignore(Menu_viewer.tools_menu_filter#connect#activate display_filter_list)

end


(***********************************************
	     SOURCE CONTROLLER
***********************************************)

module Source_controller =
struct

  open Source_viewer

    (* default is *)
  let source_file = ref ""
  let load_source () =
    if !source_file = "" then
      source_file := !Parameters.program_name ^ ".ml";
    load_source_file !source_file


  (* here comes the events and breakpoints icons handling *)

  open Ocabug_event_boxes

  let break_callback ev = fun () -> Breakpoints.new_breakpoint ev
  let event_callback n = fun () -> Breakpoints.remove_breakpoint n

  let event_break_callback n ev b =
    set_or_remove_break
      (* break number incremented because call to new_event is not done yet *)
      n
      (break_callback ev)
      event_callback;
    true

  let show_events_from_module mdl =
    current_event_boxes := Hashtbl.find event_boxes_by_module mdl;
    list_iteri
      (fun n (_, ebox) ->
	let offset = n + (Events.get_pos ebox.event).Lexing.pos_cnum in
	let test_ebox = GBin.event_box ~show:true () in
	let pix = pixbuf_from_ebox ebox in
	ebox.image <- GMisc.image ~pixbuf:pix ~packing:test_ebox#add ();
	ignore(
	  source_box#add_child_at_anchor 
	    (test_ebox :> GObj.widget) 
	    (source_buffer#create_child_anchor
	       (source_buffer#get_iter (`OFFSET offset))));
	ignore(
	  test_ebox#event#connect#button_press
	    ~callback:(event_break_callback n ebox.event))
      )
      (Hashtbl.find event_boxes_by_module mdl)

  let load_events () =
    List.iter (fun m ->
	add_eboxes m (Symbols.events_in_module m)
	  event_break_callback event_pixbuf)
      !Symbols.modules;
    (*show_events_from_module
      (List.find (fun m -> not (List.mem m !Symbols.exclude_modules))
	 !Symbols.modules)*)
    
    
(*
  let show_event_in_source n ev =
    let offset = (Events.get_pos ev).Lexing.pos_cnum + n in
    let ebox = GBin.event_box ~show:true () in
    ()
*)
end




(**********************************************
	      MODULES CONTROLLER
**********************************************)


module Modules_controller =
struct

  open Source_controller
  open Symbols

  let current_mod = ref ""

  let set_module m () =
    if m <> !current_mod && List.mem m !active_modules then
      begin
	current_mod := m;
	(*print_endline m;*)
	source_file := Source.source_of_module Lexing.dummy_pos m;
	load_source ();
	show_events_from_module m
      end

  let load_modules () =
    exclude_modules :=
      List.map
      (fun s -> String.capitalize (Filename.chop_extension s))
      (Ocabug_misc.dir_content (Config.standard_library));
    active_modules := 
      List.filter
	(fun m -> not (List.mem m !exclude_modules))
	!modules;
    List.iter
      (fun mod_name ->
	let item = Modules_combo.make_arrow_label ~label:mod_name ~string:mod_name in
	ignore (item#connect#select ~callback:(set_module mod_name))
      )
      !active_modules;
    if !active_modules <> [] then current_mod := List.hd !active_modules;
    show_events_from_module !current_mod;
    Modules_combo.switch_module_callback := set_module;

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


let window_show () =
  window#show ();
  GMain.Main.main ()


let show_ui () =
  Program_management.ensure_loaded ();
  Buttons_controller.set_icons_callbacks ();
  Buttons_controller.set_menu_callbacks ();
  Source_controller.load_source ();
  Source_controller.load_events ();
  Modules_controller.load_modules ();
  Toplevel_controller.connect ();
  window_show ()
  
