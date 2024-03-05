(* This feature is turned on/off by Makefile variable SUPPORT_COMPARISON. *)

let f fmt = Lib_test.f __FILE__ fmt;;

let x = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"LATIN1" in
let y = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"LATIN1" in
match x = y with
| exception (Invalid_argument _ as exn) ->
	let _: string = f __LINE__ "%s" (Printexc.to_string exn) in
	Printf.eprintf "%s: configured as no comparison.\n" Sys.argv.(0);
	exit 1
| _ as checked ->
	assert (checked |> f __LINE__ "%B");
	let hash_x = Hashtbl.hash x |> f __LINE__ "%.8x" in
	let hash_y = Hashtbl.hash y |> f __LINE__ "%.8x" in
	assert (hash_x = hash_y);;

let x = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"ASCII" in
let y = Iconv.iconv_open ~tocode:"ASCII" ~fromcode:"LATIN1" in
let checked = x <> y |> f __LINE__ "%B" in
assert checked;
let hash_x = Hashtbl.hash x |> f __LINE__ "%.8x" in
let hash_y = Hashtbl.hash y |> f __LINE__ "%.8x" in
assert (hash_x <> hash_y);;

(* report *)

prerr_endline "ok";;
