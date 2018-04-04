let u = Uchar.fromInt 97

let () = match%uchar u with
  | 'a' -> print_endline "this works"
  | 1 -> print_endline "compile"
  | _ -> print_endline "catch all"

