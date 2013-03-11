open Unix

let window =   
  let w = GWindow.window 
    ~title:"OCabug"
    ~width:800
    () in 
  ignore(w#connect#destroy ~callback:GMain.Main.quit); 
  w

let (fd_in, fd_out) = pipe ()
let (ui_in, ui_out) = 
  Unix.in_channel_of_descr fd_in, 
  Unix.out_channel_of_descr fd_out

(*
  vbox => Text + input

  Pour l'input => complétion ça peut être cool
*)
class command_invite pack_loc =
object (self)
  
  val buffer = GText.buffer ~text:"<ocamldebug output>" ()

  method pack () =
    let vbox = GPack.vbox ~packing:pack_loc#add () in
    ignore(GText.view ~packing:vbox#add ~height:200 ~editable:false ~buffer:buffer ~cursor_visible:false ());
    ignore(Thread.create (fun () -> while true do 
	let str = input_line ui_in in 
	buffer#set_text ((buffer#get_text ())^"\n"^str)
      done)());
    ignore(GMisc.separator `HORIZONTAL ~packing:vbox#add ());
    let entry = GEdit.entry ~text:"" ~packing:vbox#add ~editable:true () in
    let button = GButton.button ~label:"envoi" ~packing:vbox#add () in
    let send_info () = Printf.fprintf ui_out "%s\n%!" entry#text in
    button#connect#clicked ~callback:(send_info)
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
    let ic = open_in "ui_starter.ml" in
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
let invite = (new command_invite vbox)#pack ()

let show_ui () =
  load_source_file "test.ml";
  window#show ();
  GMain.Main.main ()


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
