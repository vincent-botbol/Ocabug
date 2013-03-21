open Ocabug_misc

let window =   
  let w = GWindow.window 
    ~title:"OCabug"
    ~width:800
    () in 
  ignore(w#connect#destroy ~callback:(fun () -> GMain.Main.quit (); exit 0));
  w


(*
  vbox => Text + input

  Pour l'input => complétion ça peut être cool
*)

exception Commande_invite

class command_invite pack_loc =
object (self)

  val end_response = '\003'

  val buffer = GText.buffer ~text:"<ocamldebug output>\n" ()

  method write_buffer str = buffer#insert str

  method send entry () =
    Printf.printf "Msg sent\n";
    begin
      try
	ignore (Command_line.interprete_line Format.std_formatter entry#text);
      with
	| Command_line.Command_line -> Printf.printf "command line exn caught\n%!"
	| Debugger_config.Toplevel -> Printf.printf "Toplevel exn caught\n%!"
    end;
    (* to know when to stop reading *)
    Printf.fprintf Socket_config.outchan "%c" '\003';
    entry#set_text "";
    flush Socket_config.outchan;
    let answer = my_input_line Socket_config.pipe_in in
    if answer <> "" then
      self#write_buffer (answer^"\n")

  method pack () =
    let vbox = GPack.vbox ~packing:pack_loc#add () in
    let sw = GBin.scrolled_window ~packing:vbox#add 
		    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
    in
    ignore(GText.view ~packing:sw#add ~height:200 ~editable:false ~buffer:buffer ~cursor_visible:false ());
    ignore(GMisc.separator `HORIZONTAL ~packing:vbox#add ());
    let entry = GEdit.entry ~text:"" ~packing:vbox#add ~editable:true () in
    let button = GButton.button ~label:"SEND" ~packing:vbox#add () in
    button#connect#clicked ~callback:(self#send entry)
    

end

  

let vbox = GPack.vbox ~packing:window#add ()

(********************** 
SOURCE VIEWER
**********************)

let lang_mime_type = "text/x-ocaml"
let use_mime_type = false
let font_name = "Monospace 10"

let language_manager = GSourceView2.source_language_manager ~default:true

let lang =
  match language_manager#guess_language ~content_type:lang_mime_type () with
    | None -> failwith (Printf.sprintf "no language for %s" lang_mime_type)
    | Some lang -> lang

let source_box = let sw = GBin.scrolled_window ~packing:vbox#add 
		   ~height:300 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
		 in let gsw = GSourceView2.source_view ~packing:sw#add ~editable:false 
		 ~show_line_numbers:true
		 ~right_margin_position:30
		 ~smart_home_end:`ALWAYS
		 ~cursor_visible:true () in
		    gsw#source_buffer#set_highlight_matching_brackets true;
		    gsw#set_show_line_marks true;
		    gsw#source_buffer#set_language (Some lang);
		    gsw#source_buffer#set_highlight_syntax true;
		    gsw
		 
let load_source_file filename =
  let text =
    let ic = open_in filename in
    let size = in_channel_length ic in
    let buf = String.create size in
    really_input ic buf 0 size;
    close_in ic;
    buf
  in 
  source_box#source_buffer#set_text text

(*********************
END SOURCE
**********************)


let () = ignore(GMisc.separator `HORIZONTAL ~packing:vbox#add ())
let invite =
  let cmd_inv = new command_invite vbox in
  ignore (cmd_inv#pack ());
  cmd_inv

let write str =
  invite#write_buffer str

let make_arrow_label combo ~label ~string =
  let item = GList.list_item () in (* no packing here, it blocks GTK *)
  let hbox = GPack.hbox ~spacing:3 ~packing:item#add () in
  ignore(GMisc.arrow ~kind:`RIGHT ~shadow:`OUT ~packing:hbox#pack ());
  ignore(GMisc.label ~text:label ~packing:hbox#pack ());
  combo#set_item_string item string;
  combo#list#add item;
  item


let combo = GEdit.combo ~packing:vbox#add ()

let load_modules_combo modules = 
  (* Load les events *)
  List.iter (fun mod_item -> make_arrow_label combo ~label:mod_item ~string:mod_item; ())
    modules;
  print_endline "test";
  ()

let show_ui () =
  (*Unix.connect Socket_config.debugger_socket (Unix.ADDR_UNIX Socket_config.ocabug_socket_name);*)
  Printf.printf "%s\n%!" !Parameters.program_name;
  let suffixed_name = !Parameters.program_name^".ml" in
  if Sys.file_exists suffixed_name then
    load_source_file suffixed_name
  else
    Printf.printf "File not found : %s\n%!" suffixed_name;
  Program_management.ensure_loaded ();
  load_modules_combo !Symbols.modules;
  window#show ();
  GMain.Main.main ()

(*
let () = show_ui ()
*)
(*let load_file this buffer =
  let path = (match this#filename with Some s -> s 
    | _ -> raise Exit) in
  let ic = open_in path in 
  let str = ref "" in
  (try
     while true do str:=!str^"\n"^(input_line ic) done;
   with _ -> ());
  close_in ic; 
  buffer#set_text !str
*)
(*  let file_chooser =
    GFile.chooser_button ~packing:box1#pack
      ~action:`OPEN
      ()*)
  (*
  let buffer = GText.buffer () in
  let t = GText.view ~buffer:buffer ~width:100 ~height:100 ~packing:box1#pack () in
  file_chooser#connect#selection_changed 
    (fun () -> load_file file_chooser buffer);*)
