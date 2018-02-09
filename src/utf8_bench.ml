
let time f x =
  let t = Unix.gettimeofday () in
  (* ignore (Sys.opaque_identity (f x)); *)
  ignore (f x);
  let t2 = Unix.gettimeofday () in
  let diff = t2 -. t in
  print_endline @@ (string_of_float (diff *. 1000.)) ^ "ms"

let nihon = "本"

let chinese = "你好，中华民族 hei"

let l = String.length chinese
let s = "pobrecito pobrecito pobrecito"

let b = Bytes.of_string chinese

let runs = 1_000_000

let test () =
  for _i = 1 to runs do
    ignore (Utf8.validString s);            
  done

let () =
  time test ();
