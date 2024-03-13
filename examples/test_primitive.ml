let f fmt = Lib_test.f __FILE__ fmt;;
let fa = Lib_test.fa __FILE__;;

let output_result oc x = (
	output_string oc (
		match x with
		| `ok -> "`ok"
		| `overflow -> "`overflow"
		| `illegal_sequence -> "`illegal_sequence"
	)
);;

open Iconv;;

let c = iconv_open ~tocode:"UTF-32BE" ~fromcode:"UTF-8" in
set_force_substitute c true;
(* iconv *)
let fields = {
	inbuf = "A";
	inbuf_offset = 0;
	inbytesleft = 1;
	outbuf = Bytes.create 4;
	outbuf_offset = 0;
	outbytesleft = 4
}
in
let r = iconv c fields false |> fa __LINE__ output_result in
assert (r = `ok);
assert (fields.inbytesleft |> f __LINE__ "%d" = 0);
assert (fields.outbytesleft |> f __LINE__ "%d" = 0);
assert (Bytes.to_string fields.outbuf |> f __LINE__ "%S" = "\x00\x00\x00A");
let fields = {
	inbuf = "A";
	inbuf_offset = 0;
	inbytesleft = 1;
	outbuf = Bytes.create 4;
	outbuf_offset = 0;
	outbytesleft = 3
}
in
let r = iconv c fields false |> fa __LINE__ output_result in
assert (r = `overflow);
assert (fields.inbytesleft |> f __LINE__ "%d" = 1);
assert (fields.outbytesleft |> f __LINE__ "%d" = 3);
let fields = {
	inbuf = "\xed\xa0\x80";
	inbuf_offset = 0;
	inbytesleft = 3;
	outbuf = Bytes.create 256;
	outbuf_offset = 0;
	outbytesleft = 256 (* without worrying about overflow *)
}
in
let r = iconv c fields true |> fa __LINE__ output_result in
assert (
	r = `illegal_sequence (* Citrus, glibc *)
		&& fields.inbytesleft |> f __LINE__ "%d" = 3
		&& fields.outbytesleft |> f __LINE__ "%d" = 256
	|| r = `ok (* GNU libiconv *)
		&& fields.inbytesleft |> f __LINE__ "%d" = 0
		&& fields.outbytesleft |> f __LINE__ "%d" = 252
		&& Bytes.sub_string fields.outbuf 0 fields.outbuf_offset
			= "\x00\x00\xff\xfd" (* U+fffd replacement character *)
);
(* iconv_substitute *)
let fields = {
	inbuf = "A";
	inbuf_offset = 0;
	inbytesleft = 1;
	outbuf = Bytes.create 4;
	outbuf_offset = 0;
	outbytesleft = 4
}
in
let r = iconv_substitute c fields false |> fa __LINE__ output_result in
assert (r = `ok);
assert (fields.inbytesleft |> f __LINE__ "%d" = 0);
assert (fields.outbytesleft |> f __LINE__ "%d" = 0);
assert (Bytes.to_string fields.outbuf |> f __LINE__ "%S" = "\x00\x00\x00A");
let fields = {
	inbuf = "A";
	inbuf_offset = 0;
	inbytesleft = 1;
	outbuf = Bytes.create 4;
	outbuf_offset = 0;
	outbytesleft = 3
}
in
let r = iconv_substitute c fields false |> fa __LINE__ output_result in
assert (r = `overflow);
assert (fields.inbytesleft |> f __LINE__ "%d" = 1);
assert (fields.outbytesleft |> f __LINE__ "%d" = 3);
let fields = {
	inbuf = "\xed\xa0\x80";
	inbuf_offset = 0;
	inbytesleft = 3;
	outbuf = Bytes.create 12;
	outbuf_offset = 0;
	outbytesleft = 12 (* 3 * length of substitute *)
}
in
let r = iconv_substitute c fields true |> fa __LINE__ output_result in
assert (r = `ok);
assert (fields.inbytesleft |> f __LINE__ "%d" = 0);
assert (
	fields.outbytesleft |> f __LINE__ "%d" = 0 (* Citrus, glibc *)
		&& Bytes.to_string fields.outbuf |> f __LINE__ "%S"
			= "\x00\x00\x00?\x00\x00\x00?\x00\x00\x00?"
	|| fields.outbytesleft = 8 (* GNU libiconv *)
		&& Bytes.sub_string fields.outbuf 0 fields.outbuf_offset |> f __LINE__ "%S"
			= "\x00\x00\xff\xfd" (* U+fffd replacement character *)
);

(* report *)

prerr_endline "ok";;
