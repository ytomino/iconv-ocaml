let f fmt = Lib_test.f __FILE__ fmt;;

let iconv = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"UTF-8" in
let m = Iconv.min_sequence_in_fromcode iconv |> f __LINE__ "%d" in
assert (m = 1);
Iconv.set_substitute iconv "-"; (* That customize substitution text. *)
if Iconv.unexist iconv = `auto then (
	let x = Iconv.iconv_string iconv "\xC4\x80" |> f __LINE__ "%S" in
	assert (x = "?")
	(* When Citrus internally substitution is active, that can skip best length,
	   but application can not customize substitution text. *)
);
Iconv.set_unexist iconv `illegal_sequence;
let x = Iconv.iconv_string iconv "\xC4\x80" |> f __LINE__ "%S" in
assert (x = "--");;
	(* That is customized text.
	   A fallback handler is needed to return "?". *)

let iconv = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"UTF-16BE" in
let m = Iconv.min_sequence_in_fromcode iconv |> f __LINE__ "%d" in
assert (m = 2);
let x = Iconv.iconv_string iconv "\x01\x00" |> f __LINE__ "%S" in
assert (x = "?");;

let iconv = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"UTF-32BE" in
let m = Iconv.min_sequence_in_fromcode iconv |> f __LINE__ "%d" in
assert (m = 4);
let x = Iconv.iconv_string iconv "\x00\x00\x01\x00" |> f __LINE__ "%S" in
assert (x = "?");;

let iconv = Iconv.iconv_open ~tocode:"UTF-8" ~fromcode:"UTF-16BE" in
let x = Iconv.iconv_string iconv "\x01\x00" |> f __LINE__ "%S" in
assert (x  = "\xC4\x80");
let x = Iconv.iconv_string iconv "\xDF\xFF" |> f __LINE__ "%S" in
assert (x = "?");
Iconv.set_substitute iconv "";
let x = Iconv.iconv_string iconv "\xDF\xFF" |> f __LINE__ "%S" in
assert (x = "");;

let iconv = Iconv.iconv_open ~tocode:"UTF-16BE" ~fromcode:"UTF-8" in
let x = Iconv.iconv_string iconv "\xFF" |> f __LINE__ "%S" in
assert (x = "\x00\x3F");
Iconv.set_substitute iconv "";
let x = Iconv.iconv_string iconv "\xFF" |> f __LINE__ "%S" in
assert (x = "");;

let iconv = Iconv.iconv_open ~tocode:"UTF-16BE" ~fromcode:"UTF-32BE" in
let x = Iconv.iconv_string iconv "\x00\x00\x01\x00" |> f __LINE__ "%S" in
assert (x = "\x01\x00");;

let iconv = Iconv.iconv_open ~tocode:"UTF-32BE" ~fromcode:"UTF-8" in
let x = Iconv.iconv_string iconv "\xFF" |> f __LINE__ "%S" in
assert (x = "\x00\x00\x00\x3F");
Iconv.set_substitute iconv "";
let x = Iconv.iconv_string iconv "\xFF" |> f __LINE__ "%S" in
assert (x = "");;

let buf = Buffer.create 256 in
let w =
	Iconv.Out_iconv.open_out ~tocode:"UTF-32BE" ~fromcode:"UTF-16BE"
		(Buffer.add_substring buf)
in
let s = "\xD8\x7E\xDC\x00" in (* U+2F800 *)
Iconv.Out_iconv.output_substring w s 0 2; (* high surrogate *)
let len = Buffer.length buf |> f __LINE__ "%d" in
assert (len = 0); (* pending *)
Iconv.Out_iconv.output_substring w s 3 1; (* low surrogate but truncated *)
let len = Buffer.length buf |> f __LINE__ "%d" in
assert (len = 0); (* pending *)
Iconv.Out_iconv.end_out w;
let x = Buffer.contents buf |> f __LINE__ "%S" in
assert (x = "\x00\x00\x00\x3F\x00\x00\x00\x3F");
Buffer.clear buf;
Iconv.Out_iconv.reset_out w;
Iconv.Out_iconv.output_substring w s 0 4;
Iconv.Out_iconv.end_out w;
let x = Buffer.contents buf |> f __LINE__ "%S" in
assert (x = "\x00\x02\xF8\x00");;

(* report *)

prerr_endline "ok";;
