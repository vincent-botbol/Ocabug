

let quit () =
  GMain.Main.quit (); exit 0  

let window =   
  let w = GWindow.window 
    ~title:"OCabug"
    ~width:800
    () in 
  ignore(w#connect#destroy ~callback:quit);
  w

exception Commande_invite



let vbox = GPack.vbox ~packing:window#add ()



(**********************************
		 MENU
**********************************)


module Menu_viewer =
struct
  open GdkKeysyms  
    
  let packer = GPack.fixed 
    ~packing:vbox#add ()
    
  let menu_bar = GMenu.menu_bar ~packing:packer#add ()
  let factory = new GMenu.factory menu_bar

  let () = window#add_accel_group factory#accel_group
    
  let file_menu = factory#add_submenu "File"
  let ff = new GMenu.factory file_menu ~accel_group:factory#accel_group
  let tools_menu = factory#add_submenu "Tools"
  let tf = new GMenu.factory tools_menu ~accel_group:factory#accel_group
  let help_menu = factory#add_submenu "Help"
  let hf = new GMenu.factory help_menu ~accel_group:factory#accel_group
    

  (* [menu_item]#connect#activate ~callback:.. *)
  let file_menu_quit = ff#add_item ~key:_W ~callback:quit "Quit" 
    
  let tools_menu_printer = tf#add_item "Set printer"
    
  let help_menu_about = hf#add_item 
    ~callback:(fun () -> Printf.printf "Ocabug - credits : Mathieu Chailloux, Vincent Botbol\n%!")
    "About..."
    
    
end


(**********************************
		 ICONS
**********************************)


module Buttons_viewer =
struct
  
  let packer = GPack.hbox ~packing:vbox#add ()

  type button_kind =
    Step | Backstep | Run | Reverse | Bigstep
	
  type button_elem =
    (* kind, label, tooltip *)
    button_kind * string * string 
      
  let get_icon_path = function
    | Step -> "../img/icons/step.png"
    | Backstep -> "../img/icons/backstep.png"
    | Run -> "../img/icons/run.png"
    | Reverse -> "../img/icons/temp/bullet_left.png"
    | Bigstep -> "../img/icons/temp/forward_green.png"

  let toolbar = GButton.toolbar ~orientation:`HORIZONTAL
      ~style:`BOTH_HORIZ ~border_width:5
      ~height:40
      ~packing:packer#add ()
      
  let buttons = 
    List.map (fun ((kind, label, tooltip) : button_elem) ->
      let b = toolbar#insert_button
	~text:label
	~tooltip:tooltip
	~icon:((GMisc.image ~file:(get_icon_path kind) ()) :> GObj.widget)
	() in
      b#set_focus_on_click false;
      (kind,b)
    )
      [ (Run, "Run", "Continue to the next breakpoint")
      ; (Reverse, "Reverse", "Go to the beggining of the program") 
      ; (Backstep, "Backstep", "Revert to the previous event")
      ; (Step, "Step", "Go to the next event")      
      ; (Bigstep, "Big step", "Go the next event skipping the standard lib")
      ]

end

(********************************************
	      COMBO MODULES
********************************************)

module Modules_combo =
struct

  let packer = GBin.alignment
    ~padding:(2,2,2,2)
    ~width:10
    ~packing:(Buttons_viewer.packer#add) ()

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


(*******************************************
	      SOURCE VIEWER
*******************************************)

module Source_viewer = 
struct
  
  let packer = GBin.frame
    ~border_width:5
    ~shadow_type:`IN
    ~packing:vbox#add ()
  
  let breakpoint_pixbuf = GdkPixbuf.from_file "../img/breakpoint.png"
  let event_pixbuf = GdkPixbuf.from_file "../img/icons/temp/bullet_plus.png"
  let breakpoint_pixbuf2 = GdkPixbuf.from_file "../img/icons/temp/decline.png"
  let event_pixbuf2 = GdkPixbuf.from_file "../img/icons/temp/control_add.png"

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
      
  (* - is it useful ? 
     - maybe
  *)
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


(*************************************************
		 TOPLEVEL
**************************************************)




module Command_invite =
struct

  let packer = 
    let frame = GBin.frame
      ~border_width:5
      ~shadow_type:`IN
      ~packing:vbox#add () in
    GPack.vbox ~packing:frame#add ()
      
  let end_response = '\003'

  let buffer = GText.buffer ~text:"[OCabug]\n" ()

  let sw = let align = GBin.alignment 
	     ~packing:packer#add () in
	   GBin.scrolled_window ~packing:align#add 
	     ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()

  let adjust_window () =
    let adj = sw#vadjustment in
    adj#set_value (adj#upper -. adj#page_size)

  let last_command = ref ""

  let hbox = GPack.hbox ~packing:packer#add ()

  let entry = GEdit.entry ~text:"" ~packing:hbox#add ~editable:true ~width:700 ()

  let button =
    let align = GBin.alignment 
      ~packing:hbox#add () in
    GButton.button ~label:"Send" ~packing:align#add ()
      
  let text = GText.view ~packing:sw#add ~height:200 ~editable:false ~buffer:buffer ~cursor_visible:false ()

  let write_buffer str = 
    buffer#insert str

(*     
       let trim s =
       let is_space = function
       | ' ' | '\012' | '\n' | '\r' | '\t' -> true
       | _ -> false in
       let len = String.length s in
       let i = ref 0 in
       while !i < len && is_space (String.get s !i) do
       incr i
       done;
       let j = ref (len - 1) in
      while !j >= !i && is_space (String.get s !j) do
       decr j
       done;
      if !i = 0 && !j = len - 1 then
       s
       else if !j >= !i then
       String.sub s !i (!j - !i + 1)
       else
       ""
*)

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
