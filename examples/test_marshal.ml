open Iconv;;
open Iconv_pp;;

let marshal (x: 'a) = (
	let s = Marshal.to_string x [] in
	Marshal.from_string s 0
);;

let check_marshal (x: iconv_t) = (
	let y = marshal x in
	iconv_tocode x = iconv_tocode y && iconv_fromcode y = iconv_fromcode y
);;

let c = iconv_open ~tocode:"sjis" ~fromcode:"euc-jp" in
match check_marshal c with
| exception Invalid_argument message ->
	Printf.eprintf "%s: configured to no serialization, \"%s\"\n" Sys.argv.(0)
		message;
	exit 1
| checked ->
	assert checked;
	Printf.eprintf "ok\n";;
