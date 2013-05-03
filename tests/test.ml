open Printers

let rec fact = function 0 -> 1 | n -> n * (fact (n - 1))

let l = [1;2;3]

let _ =
  let x = 1+2 in
  fact x

module M =
  struct
    let a = A
    let b = B
    let c = C
    let f x = print_t x
  end

let _ =
  begin
    let a = M.a in
    let b = M.b in
    let c = M.c in
    M.f b;
    print_string "hehe"
  end
