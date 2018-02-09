type utf8Test = {
  uchar: Uchar.t;
  str: string;
  size: int;
}

let fi = Uchar.fromInt

let utf8Tests = [
  {uchar = fi 0x00; str = "\x00"; size = 1};
  {uchar = fi 0x01; str = "\x01"; size = 1};
  {uchar = fi 0x7e; str = "\x7e"; size = 1};
  {uchar = fi 0x7f; str = "\x7f"; size = 1};
  {uchar = fi 0x0080; str = "\xc2\x80"; size = 2};
  {uchar = fi 0x0081; str = "\xc2\x81"; size = 2};
  {uchar = fi 0x00bf; str = "\xc2\xbf"; size = 2};
  {uchar = fi 0x00c0; str = "\xc3\x80"; size = 2};
  {uchar = fi 0x00c1; str = "\xc3\x81"; size = 2};
  {uchar = fi 0x00c8; str = "\xc3\x88"; size = 2};
  {uchar = fi 0x00d0; str = "\xc3\x90"; size = 2};
  {uchar = fi 0x00e0; str = "\xc3\xa0"; size = 2};
  {uchar = fi 0x00f0; str = "\xc3\xb0"; size = 2};
  {uchar = fi 0x00f8; str = "\xc3\xb8"; size = 2};
  {uchar = fi 0x00ff; str = "\xc3\xbf"; size = 2};
  {uchar = fi 0x0100; str = "\xc4\x80"; size = 2};
  {uchar = fi 0x07ff; str = "\xdf\xbf"; size = 2};
  {uchar = fi 0x0400; str = "\xd0\x80"; size = 2};
  {uchar = fi 0x0800; str = "\xe0\xa0\x80"; size = 3};
  {uchar = fi 0x0801; str = "\xe0\xa0\x81"; size = 3};
  {uchar = fi 0x1000; str = "\xe1\x80\x80"; size = 3};
  {uchar = fi 0xd000; str = "\xed\x80\x80"; size = 3};
  {uchar = fi 0xd7ff; str = "\xed\x9f\xbf"; size = 3};
  {uchar = fi 0xe000; str = "\xee\x80\x80"; size = 3};
  {uchar = fi 0xfffe; str = "\xef\xbf\xbe"; size = 3};
  {uchar = fi 0xffff; str = "\xef\xbf\xbf"; size = 3};
  {uchar = fi 0x10000; str = "\xf0\x90\x80\x80"; size = 4};
  {uchar = fi 0x10001; str = "\xf0\x90\x80\x81"; size = 4};
  {uchar = fi 0x40000; str = "\xf1\x80\x80\x80"; size = 4};
  {uchar = fi 0x10fffe; str = "\xf4\x8f\xbf\xbe"; size = 4};
  {uchar = fi 0x10ffff; str = "\xf4\x8f\xbf\xbf"; size = 4};
  {uchar = fi 0xFFFD; str = "\xef\xbf\xbd"; size = 3}
]

let surrogateRange = [
  {uchar = fi 0xFFFD; str = "\xed\xa0\x80"; size = 1};
  {uchar = fi 0xFFFD; str = "\xed\xbf\xbf"; size = 1};
]

let invalidSequences = [
  "\xed\xa0\x80\x80";
	"\xed\xbf\xbf\x80";

	"\x91\x80\x80\x80";

	"\xC2\x7F\x80\x80";
	"\xC2\xC0\x80\x80";
	"\xDF\x7F\x80\x80";
	"\xDF\xC0\x80\x80";

	"\xE0\x9F\xBF\x80";
	"\xE0\xA0\x7F\x80";
	"\xE0\xBF\xC0\x80";
	"\xE0\xC0\x80\x80";

	"\xE1\x7F\xBF\x80";
	"\xE1\x80\x7F\x80";
	"\xE1\xBF\xC0\x80";
	"\xE1\xC0\x80\x80";

	"\xED\x7F\xBF\x80";
	"\xED\x80\x7F\x80";
	"\xED\x9F\xC0\x80";
	"\xED\xA0\x80\x80";

	"\xF0\x8F\xBF\xBF";
	"\xF0\x90\x7F\xBF";
	"\xF0\x90\x80\x7F";
	"\xF0\xBF\xBF\xC0";
	"\xF0\xBF\xC0\x80";
	"\xF0\xC0\x80\x80";

	"\xF1\x7F\xBF\xBF";
	"\xF1\x80\x7F\xBF";
	"\xF1\x80\x80\x7F";
	"\xF1\xBF\xBF\xC0";
	"\xF1\xBF\xC0\x80";
	"\xF1\xC0\x80\x80";

	"\xF4\x7F\xBF\xBF";
	"\xF4\x80\x7F\xBF";
	"\xF4\x80\x80\x7F";
	"\xF4\x8F\xBF\xC0";
	"\xF4\x8F\xC0\x80";
	"\xF4\x90\x80\x80";
]

type countTest = {
  input: string;
  count: int;
}

let countTests = [
  {input = "qwerty"; count = 6 };
  {input = "🤓😮😁"; count = 3 };
  {input = "\xc2\x80"; count = 1};
  {input = "zzz\xc2\x80"; count = 4 };
]

(* valid decoding *)
let () =
  List.iter (fun t -> 
    let len = String.length t.str in
    let (u, s) = Utf8.decodeUcharInString 0 t.str len in
    assert (u = t.uchar);
    assert (s = t.size);
    let (u, s) = Utf8.decodeUchar 0 (Bytes.of_string t.str) len in
    assert (u = t.uchar);
    assert (s = t.size);
    () 
) utf8Tests

(* decoding of surrogate range *)
let () =
  List.iter (fun t ->
    let len = String.length t.str in
    let (u, s) = Utf8.decodeUcharInString 0 t.str len in
    assert (u = t.uchar);
    assert (s = t.size);
    let bts = Bytes.of_string t.str in
    let len = Bytes.length bts in
    let (u, s) = Utf8.decodeUchar 0 bts len in
    assert (u = t.uchar);
    assert (s = t.size);
  ) surrogateRange

(* encode uchar in bytes *)
let () =
  List.iter (fun t ->
    let b = Bytes.of_string t.str in
    let b2 = Bytes.make 10 '0' in
    let n = Utf8.encodeUchar b2 t.uchar in
    let b2 = Bytes.sub b2 0 n in
    assert (n = t.size);
    assert (Bytes.compare b b2 = 0);
    ()
  ) utf8Tests

let () =
  List.iter (fun str ->
    let len = String.length str in
    let (u, s) = Utf8.decodeUcharInString 0 str len in
    assert (u = Uchar.fromInt 0xFFFD);
    assert (s = 1);
    let bts = Bytes.of_string str in
    let len = Bytes.length bts in
    let (u, s) = Utf8.decodeUchar 0 bts len in
    assert (u = Uchar.fromInt 0xFFFD);
    assert (s = 1);
    (* TODO runtime decode !!!! *)
    ()
  ) invalidSequences
    

let () = 
  List.iter (fun t ->
    assert (Utf8.countInString t.input = t.count);
    assert (Utf8.count (Bytes.of_string t.input) = t.count);
  ) countTests

let () =
  List.iter (fun t ->
    assert (Utf8.valid (Bytes.of_string t.input) = true);
    assert (Utf8.validString t.input = true);
  ) countTests

let () =
  List.iter (fun str ->
    assert (Utf8.valid (Bytes.of_string str ) = false);
    assert (Utf8.validString str = false);
  ) invalidSequences


let () = print_endline "utf8_test succeeded! :) "

let () = 
  let b = Bytes.create 2 in
  let nwritten = (Utf8.encodeUchar b (Uchar.fromInt 128)) in
  assert (nwritten = 2);
  print_int (Char.code (Bytes.get b 0));
  print_newline ();
  print_int (Char.code (Bytes.get b 1));
  print_newline ()

