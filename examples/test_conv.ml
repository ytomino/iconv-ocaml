let verbose = Array.length Sys.argv > 1 && Sys.argv.(1) = "--verbose";;

open Iconv;;

let c = iconv_open ~tocode:"sjis" ~fromcode:"utf-8";;
let s = "ソースコード直書きのUTF-8文字列です\n" in
let x = iconv c s in
if verbose then print_endline (String.escaped x);
assert (
	x = "\131\\\129[\131X\131R\129[\131h\146\188\143\145\130\171\130\204UTF-8"
		^ "\149\182\142\154\151\241\130\197\130\183\n"
);;

(* report *)

Printf.eprintf "ok\n";;
