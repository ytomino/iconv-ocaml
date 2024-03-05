(* This feature is turned on/off by Makefile variable SUPPORT_SERIALIZATION. *)

let f fmt = Lib_test.f __FILE__ fmt;;

open Iconv;;
open Iconv_pp;;

let marshal (x: 'a) = (
	let s = Marshal.to_string x [] in
	Marshal.from_string s 0
);;

let check_marshal (x: iconv_t) = (
	let y = marshal x in
	tocode x = tocode y && fromcode y = fromcode y
	&& substitute x = substitute y && force_substitute x = force_substitute y
);;

let c = iconv_open ~tocode:"sjis" ~fromcode:"euc-jp" in
match check_marshal c with
| exception (Invalid_argument _ as exn) ->
	let _: string = f __LINE__ "%s" (Printexc.to_string exn) in
	Printf.eprintf "%s: configured as no serialization.\n" Sys.argv.(0);
	exit 1
| checked ->
	assert (checked |> f __LINE__ "%B");
	prerr_endline "ok";;
