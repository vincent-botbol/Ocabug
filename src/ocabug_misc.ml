open Ocabug_config

let my_input_line fd = 
  let s = " "  and  r = ref ""
  in while (ThreadUnix.read fd s 0 1 > 0) && s.[0] <> '\003' do r := !r ^s done ;
  !r
    
let my_output_line fd str =
  ignore (ThreadUnix.write fd str 0 (String.length str))

exception Failed_read_ic

let read_ic ic =
  ()

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
    Ocabug_view.write (answer^"\n");
  Ocabug_view.Command_invite.adjust_window ()

let printing_function str =
  Printf.fprintf outchan "%s%!" str
(* 
let write_answers () =
  while true do
    Ui_starter.invite#write_buffer (my_input_line Socket_config.pipe_in)
  done
*)

let list_iteri f =
  let rec aux n =
    function
      | [] -> ()
      | x::xs -> f n x; aux (n+1) xs
  in
  aux 0
