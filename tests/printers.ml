open Printf

type t =
  | A
  | B
  | C

let print_t = function
  | A -> fprintf stdout "AaA\n"
  | B -> fprintf stdout "BbB\n"
  | C -> fprintf stdout "CcC\n"
