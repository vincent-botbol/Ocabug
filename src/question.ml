open Input_handling
open Primitives
open Ocabug_misc

(* Ask user a yes or no question. *)
let yes_or_no message =
  if !interactif then
    let old_prompt = !current_prompt in
      try
        current_prompt := message ^ " ? (y or n) ";
        let answer =
          let rec ask () =
            resume_user_input ();
            let line =
              string_trim (Lexer_debug.line (Lexing.from_function read_user_input))
            in
              stop_user_input ();
              match (if String.length line > 0 then line.[0] else ' ') with
                'y' -> true
              | 'n' -> false
              | _ ->
                printing_function "Please answer y or n.";
                ask ()
          in
            ask ()
        in
          current_prompt := old_prompt;
          answer
      with
        x ->
          current_prompt := old_prompt;
          stop_user_input ();
          raise x
  else
    false
