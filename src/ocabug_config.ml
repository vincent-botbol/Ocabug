open Unix

let ocabug_socket_name =
  match Sys.os_type with
      "Win32" ->
        (Unix.string_of_inet_addr Unix.inet_addr_loopback)^
          ":"^
          (string_of_int (10000 + ((Unix.getpid ()) mod 10000)))
    | _ -> Filename.concat Filename.temp_dir_name
      ("ocabug" ^ (string_of_int (Unix.getpid ())))



(*
let initialize_sockets () =
  Unix.setsockopt ocabug_socket SO_REUSEADDR true;
  Unix.bind ocabug_socket (ADDR_UNIX ocabug_socket_name);
  Unix.listen ocabug_socket 1;
  let (sd, _) = Unix.accept ocabug_socket in
  Printf.printf "Socket initialized\n%!";
  sd
*)

let debugger_socket =
  let s = ThreadUnix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.setsockopt s SO_REUSEADDR true;
  s

let pipe_in, pipe_out = ThreadUnix.pipe ()
let outchan = Unix.out_channel_of_descr pipe_out
let inchan = Unix.in_channel_of_descr pipe_in
let formatter = Format.formatter_of_out_channel outchan



let printing_function str =
  Printf.fprintf outchan "%s%!" str

let force_print str =
  Ocabug_view.Command_invite.write_buffer str
