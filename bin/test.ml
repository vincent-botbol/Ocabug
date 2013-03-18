let rec fact = function 0 -> 1 | n -> n * (fact (n - 1))

let _ =
  let x = 1+2 in
  fact x
  
