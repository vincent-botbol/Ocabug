open Ocabug_config
open Unix
open Filename

let my_input_line fd = 
  let s = " "  and  r = ref ""
  in while (ThreadUnix.read fd s 0 1 > 0) && s.[0] <> '\003' do r := !r ^s done ;
  !r
    
let my_output_line fd str =
  ignore (ThreadUnix.write fd str 0 (String.length str))

exception Failed_read_ic

let word_from_string str =
  let i = ref 0 and n = String.length str in
  while str.[!i] <> ' ' && !i < n-1 do
    incr i
  done;
  String.sub str 0 !i

let force_read () =
  Printf.fprintf outchan "%c" '\003';
  flush outchan;
  my_input_line pipe_in

let force_write () =
  let answer = force_read () in
  if answer <> "" then
    Ocabug_view.write answer;
  Ocabug_view.Command_invite.adjust_window ()

let printing_function str =
  if str <> "" then
    Ocabug_view.write (str^"\n");
  Ocabug_view.Command_invite.adjust_window ()

let print_error = printing_function

(*
  Printf.fprintf outchan "%s%!" str;
  force_write ()
*)
(* 
let write_answers () =
  while true do
    Ui_starter.invite#write_buffer (my_input_line Socket_config.pipe_in)
  done
*)

(* classic iteri, not available before 4.0 *)
let list_iteri f =
  let rec aux n =
    function
      | [] -> ()
      | x::xs -> f n x; aux (n+1) xs
  in
  aux 0

let list_mapi f l =
  let rec aux n acc =
    function
      | [] -> acc
      | x::xs -> aux (n+1) ((f n x)::acc) xs
  in
  List.rev (aux 0 [] l)

let list_iteri2 f =
  let rec aux n l1 l2 =
    match l1, l2 with
      | [], [] -> ()
      | x::xs, y::ys -> f n x y; aux (n+1) xs ys
      | _ -> raise (Invalid_argument "list_iteri2")
  in
  aux 0

let dir_content dir =
  let file_list = ref [] in
  let handle = opendir dir in
  try
    while true do
      let file = readdir handle in
      if (check_suffix file ".ml") || (check_suffix file ".mli") then
      file_list := file :: !file_list
    done;
    failwith "come check me in dir_content !"
  with
    | End_of_file -> !file_list

(*
(* ATTENTION CONFLIT AVEC source.ml *)
let source_of_module mdl = (String.uncapitalize mdl) ^ ".ml"

let module_of_source src = 
  if (Filename.check_suffix src ".ml") ||
    (Filename.check_suffix src ".mli")
  then
    String.capitalize (chop_extension (basename src))
  else
    begin
      Printf.printf "File not found %s\n%!" src;
      failwith "come in Ocabug_misc.module_of_source"
    end
*)
