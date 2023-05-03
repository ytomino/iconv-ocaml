external min_sequence_in_fromcode: Iconv.iconv_t -> int =
	"mliconv_min_sequence_in_fromcode";;

let iconv = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"UTF-8" in
assert (min_sequence_in_fromcode iconv = 1);
assert (Iconv.iconv iconv "\xC4\x80" = "??");;
	(* A fallback handler is needed to return "?". *)

let iconv = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"UTF-16BE" in
assert (min_sequence_in_fromcode iconv = 2);
assert (Iconv.iconv iconv "\x01\x00" = "?");;

let iconv = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"UTF-32BE" in
assert (min_sequence_in_fromcode iconv = 4);
assert (Iconv.iconv iconv "\x00\x00\x01\x00" = "?");;

let iconv = Iconv.iconv_open ~tocode:"UTF-8" ~fromcode:"UTF-16BE" in
assert (Iconv.iconv iconv "\x01\x00" = "\xC4\x80");
assert (Iconv.iconv iconv "\xDF\xFF" = "?");;

let iconv = Iconv.iconv_open ~tocode:"UTF-16BE" ~fromcode:"UTF-32BE" in
assert (Iconv.iconv iconv "\x00\x00\x01\x00" = "\x01\x00");;

(* report *)

Printf.eprintf "ok\n";;
