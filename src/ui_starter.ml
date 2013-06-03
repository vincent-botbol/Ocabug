open Ocabug_misc

let window =   
  let w = GWindow.window 
    ~title:"OCabug"
    ~width:800
    () in 
  ignore(w#connect#destroy ~callback:(fun () -> GMain.Main.quit (); exit 0));
  w


(********************
COMMAND INVITE
********************)

exception Commande_invite

class command_invite pack_loc =
object (self)

  val end_response = '\003'

  val buffer = GText.buffer ~text:"[OCabug]\n" ()
  val sw = GBin.scrolled_window ~packing:pack_loc#add 
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () 

  val last_command = ref ""

  method write_buffer str = buffer#insert str

  method send entry () =
    Printf.printf "[DEBUG] Msg sent\n";
    begin
      try
	ignore (Command_line.interprete_line Format.std_formatter entry#text);
      with
	| Command_line.Command_line -> Printf.printf "command line exn caught\n%!"
	| Debugger_config.Toplevel -> Printf.printf "Toplevel exn caught\n%!"
    end;
    (* to know when to stop reading *)
    Printf.fprintf Ocabug_config.outchan "%c" '\003';
    last_command := entry#text; entry#set_text "";
    flush Ocabug_config.outchan;
    let answer = my_input_line Ocabug_config.pipe_in in
    if answer <> "" then
      self#write_buffer answer;
    (* TODO : Parse de l'event pour se positionner dans le source *)
    
    (* Suivi automatique du scrolling *)
    let adj = sw#vadjustment in
    adj#set_value (adj#upper -. adj#page_size)
    

  method private key_callback entry key =
    let value = GdkEvent.Key.keyval key in
    if value = 65421 || value = 65293 then 
      begin self#send entry (); false; end
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

  method pack () =
    ignore(GText.view ~packing:sw#add ~height:200 ~editable:false ~buffer:buffer ~cursor_visible:false ());
    let hbox = GPack.hbox ~packing:pack_loc#add () in
    let entry = GEdit.entry ~text:"" ~packing:hbox#add ~editable:true ~width:700 () 
    and button = GButton.button ~label:"SEND" ~packing:hbox#add () in
    ignore(GMisc.separator `HORIZONTAL ~packing:pack_loc#add ());
    (* validation sur entrée *)
    ignore(entry#event#connect#key_press ~callback:(self#key_callback entry));
    button#connect#clicked ~callback:(self#send entry)

end

(********************
END COMMAND INVITE
********************)

(********************** 
SOURCE VIEWER
**********************)
class source_viewer packer = 
object(self)

  
  val breakpoint_pixbuf = GdkPixbuf.from_xpm_data Ocabug_icons.breakpoint

  val source_box = 
    let lang = 
      let lang_mime_type = "text/x-ocaml"
      and language_manager = GSourceView2.source_language_manager ~default:true in
      match language_manager#guess_language ~content_type:lang_mime_type () with
      | None -> failwith (Printf.sprintf "no language for %s" lang_mime_type)
      | Some lang -> lang in
    let sw = GBin.scrolled_window ~packing:packer#add 
      ~height:300 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
    in let gsw = GSourceView2.source_view ~packing:sw#add ~editable:false 
	 ~show_line_numbers:true
	 ~right_margin_position:30
	 ~smart_home_end:`ALWAYS
	 ~cursor_visible:true () in
       gsw#source_buffer#set_highlight_matching_brackets true;
       gsw#set_show_line_marks true;
       gsw#source_buffer#set_language (Some lang );
       gsw#source_buffer#set_highlight_syntax true;
       gsw#source_buffer#set_text "(* No module loaded *)";
       gsw
			
  method source_buffer = source_box#source_buffer
      		 
  method load_source_file filename =
    let text =
      let ic = open_in filename in
      let size = in_channel_length ic in
      let buf = String.create size in
      really_input ic buf 0 size;
      close_in ic;
      buf
    in 
    source_box#source_buffer#set_text text

  (** breakpoints **)
  val break_cpt = ref 0

  method breakpoint_callback dbg_ev b =
    print_endline "Breakpoint clické"; true
      

  method add_breakpoint (dbg_ev : Instruct.debug_event) : unit =
  let offset = dbg_ev.Instruct.ev_loc.Location.loc_start.Lexing.pos_cnum + !break_cpt in
  incr break_cpt;
  let ebox = GBin.event_box ~show:true () in
  ignore(GMisc.image ~pixbuf:breakpoint_pixbuf ~packing:ebox#add ());
  ignore(ebox#event#connect#button_press ~callback:(self#breakpoint_callback dbg_ev));
  ignore(
    source_box#add_child_at_anchor 
      (ebox :> GObj.widget) 
      (self#source_buffer#create_child_anchor (self#source_buffer#get_iter (`OFFSET offset))))   
    
  (* debug pour test *)
  method load_breakpoints mod_name =
  let events =  Symbols.events_in_module mod_name in
  break_cpt:=0;
  List.iter (self#add_breakpoint) events
      
    
end

(*********************
END SOURCE
**********************)

(********************
MODULE SELECTION
*********************)

class modules_combo packer =
object(self)

  val combo = GEdit.combo ~packing:packer#add ()
  val mutable current_mod = ""
  val mutable item_list = []

  method set_module m () =
    if m <> current_mod then
      begin
	current_mod <- m;
	print_endline m
      end

  method make_arrow_label ~label ~string =
    let item = GList.list_item () in (* no packing here, it blocks GTK *)
    let hbox = GPack.hbox ~spacing:3 ~packing:item#add () in
    ignore(GMisc.arrow ~kind:`RIGHT ~shadow:`OUT ~packing:hbox#pack ());
    ignore(GMisc.label ~text:label ~packing:hbox#pack ());
    combo#set_item_string item string;
    combo#list#add item;
    ignore (item#connect#select ~callback:(self#set_module string))

  method load_combo_modules modules = 
    (* Load les events *)
    List.iter (fun mod_item -> self#make_arrow_label ~label:mod_item ~string:mod_item)
      modules;
    print_endline "test"

end

(********************
END MODULE SELECTION
*********************)

(** TODO : faire les callbacks *)
(** Rajouter les icones qui nous interessent *)
(**********************
ICONS
**********************)

let run_callback () = 
  Printf.printf "run pressed\n%!"

let backstep_callback () = 
  Printf.printf "backstep pressed\n%!"

let step_callback () = 
  Printf.printf "step pressed\n%!"

let add_icons packer =
  let paths_and_callbacks = [ ("../img/backstep.png", backstep_callback)
			   ; ("../img/run.png", run_callback)
			   ; ("../img/step.png", step_callback) ]  in
  List.iter
    (fun (path, callback) ->
      let button = GButton.button ~packing:packer#add () in
      button#set_image ((GMisc.image ~file:path ()) :> GObj.widget);
      ignore(button#connect#clicked callback))
    paths_and_callbacks

(**********************
END ICONS
**********************)

(**********************
INSTANCIATION & PACKING
**********************)
(** vbox windows *)
let vbox = GPack.vbox ~packing:window#add ()

(** icons *)
let () =
  let icons_hbox = GPack.hbox ~packing:vbox#add () in
  add_icons icons_hbox

(** separator source *)
let () = ignore(GMisc.separator `HORIZONTAL ~packing:vbox#add ())

let source_box = new source_viewer vbox

(** separator source/modules *)
let () = ignore(GMisc.separator `HORIZONTAL ~packing:vbox#add ())

(** combo box modules *)
let combo_modules = new modules_combo vbox

(** text input/output *)
let invite =
  let cmd_inv = new command_invite vbox in
  ignore (cmd_inv#pack ());
  cmd_inv

(**********************
END INSTANCIATION & PACKING
**********************)

(**********************
MAIN
**********************)

let write str =
  invite#write_buffer str

let show_ui () =
  (*Unix.connect Socket_config.debugger_socket (Unix.ADDR_UNIX Socket_config.ocabug_socket_name);*)
  Printf.printf "%s\n%!" !Parameters.program_name;
  let suffixed_name = !Parameters.program_name^".ml" in
  if Sys.file_exists suffixed_name then
    source_box#load_source_file suffixed_name
  else
    Printf.printf "File not found : %s\n%!" suffixed_name;
  Program_management.ensure_loaded ();
  combo_modules#load_combo_modules !Symbols.modules;
  source_box#load_breakpoints "Test";
  window#show ();
  GMain.Main.main ()

(**********************
END MAIN
**********************)
