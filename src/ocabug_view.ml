

let window =   
  let w = GWindow.window 
    ~title:"OCabug"
    ~width:800
    () in 
  ignore(w#connect#destroy ~callback:(fun () -> GMain.Main.quit (); exit 0));
  w

exception Commande_invite



let vbox = GPack.vbox ~packing:window#add ()





(**********************************
		 ICONS
**********************************)


module Icons_viewer =
struct

  let packer = GPack.hbox ~packing:vbox#add ()

  let link_icons icons_callbacks_list =
    List.iter
      (fun (path, callback) ->
	let button = GButton.button ~packing:packer#add () in
	button#set_image ((GMisc.image ~file:path ()) :> GObj.widget);
	ignore(button#connect#clicked callback)
      )
      icons_callbacks_list

  let user_printer_button = GButton.button ~label:"user_printer" ~packing:packer#add ()

end


(** separator icons / source **)
let () = ignore(GMisc.separator `HORIZONTAL ~packing:vbox#add ())





(*******************************************
	      SOURCE VIEWER
*******************************************)


     
 
module Source_viewer = 
struct

  let packer = vbox
  
  let breakpoint_pixbuf = GdkPixbuf.from_file "../img/breakpoint.png"

  let source_box = 
    let lang = 
      let lang_mime_type = "text/x-ocaml" in
      let language_manager = GSourceView2.source_language_manager ~default:true in
      match language_manager#guess_language ~content_type:lang_mime_type () with
	| None -> failwith (Printf.sprintf "no language for %s" lang_mime_type)
	| Some lang -> lang
    in
    let sw = GBin.scrolled_window ~packing:packer#add 
      ~height:300 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
    let gsw = GSourceView2.source_view ~packing:sw#add ~editable:false 
      ~show_line_numbers:true
      ~right_margin_position:30
      ~smart_home_end:`ALWAYS
      ~cursor_visible:true ()
    in
    gsw#source_buffer#set_highlight_matching_brackets true;
    gsw#set_show_line_marks true;
    gsw#source_buffer#set_language (Some lang );
    gsw#source_buffer#set_highlight_syntax true;
    gsw#source_buffer#set_text "(* No module loaded *)";
    gsw
      
      (* is it useful ? *)
  let source_buffer = source_box#source_buffer
      		 
  let load_source_file filename =
    if Sys.file_exists filename then
      let text =
	let ic = open_in filename in
	let size = in_channel_length ic in
	let buf = String.create size in
	really_input ic buf 0 size;
	close_in ic;
	buf
      in 
      source_box#source_buffer#set_text text
    else
      Printf.printf "File not found : %s\n%!" filename

  let add_breakpoint offset =
    let ebox = GBin.event_box ~show:true () in
    ignore(GMisc.image ~pixbuf:breakpoint_pixbuf ~packing:ebox#add ());
    ignore(
      source_box#add_child_at_anchor 
	(ebox :> GObj.widget) 
	(source_buffer#create_child_anchor (source_buffer#get_iter (`OFFSET offset))));
    ebox

end
  



(** separator source/modules *)
let () = ignore(GMisc.separator `HORIZONTAL ~packing:vbox#add ())






(********************************************
	      COMBO MODULES
********************************************)

module Modules_combo =
struct

  let packer = vbox

  let combo = GEdit.combo ~packing:packer#add ()

  let make_arrow_label ~label ~string =
    let item = GList.list_item () in (* no packing here, it blocks GTK *)
    let hbox = GPack.hbox ~spacing:3 ~packing:item#add () in
    ignore(GMisc.arrow ~kind:`RIGHT ~shadow:`OUT ~packing:hbox#pack ());
    ignore(GMisc.label ~text:label ~packing:hbox#pack ());
    combo#set_item_string item string;
    combo#list#add item;
    item

end




(*************************************************
		 TOPLEVEL
**************************************************)




module Command_invite =
struct

  let pack_loc = vbox

  let end_response = '\003'

  let buffer = GText.buffer ~text:"[OCabug]\n" ()

  let sw = GBin.scrolled_window ~packing:pack_loc#add 
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()

  let last_command = ref ""

  let hbox = GPack.hbox ~packing:pack_loc#add ()

  let entry = GEdit.entry ~text:"" ~packing:hbox#add ~editable:true ~width:700 ()

  let button =
    ignore(GMisc.separator `HORIZONTAL ~packing:pack_loc#add ());
    GButton.button ~label:"SEND" ~packing:hbox#add ()

  let text = GText.view ~packing:sw#add ~height:200 ~editable:false ~buffer:buffer ~cursor_visible:false ()

  let write_buffer str = buffer#insert str

end



let write = Command_invite.write_buffer





(*
(**********************************************
		INSTANCIATIONS
***********************************************)




(** separator source/modules *)
let () = ignore(GMisc.separator `HORIZONTAL ~packing:vbox#add ())

(** combo box modules *)
let combo_modules = new modules_combo vbox

(** text input/output *)
let invite = new command_invite vbox
*)
