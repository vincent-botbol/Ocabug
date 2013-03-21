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

(*
let write_answers () =
  while true do
    Ui_starter.invite#write_buffer (my_input_line Socket_config.pipe_in)
  done
*)
