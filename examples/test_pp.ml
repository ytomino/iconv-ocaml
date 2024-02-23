let verbose = Array.length Sys.argv > 1 && Sys.argv.(1) = "--verbose";;

open Iconv;;
open Iconv_pp;;
let c = iconv_open ~tocode:"sjis" ~fromcode:"utf-8";;
set_force_substitute c true;
let x = Format.asprintf "%a@." pp_iconv c in
if verbose then Printf.printf "%s\n" (String.escaped x);
assert (
	x = "<\
		tocode = \"SHIFT_JIS\"; \
		fromcode = \"UTF-8\"; \
		substitute = \"?\";\n \
		force_substitute = true>\n"
	|| x = "<\
		tocode = \"sjis\"; \
		fromcode = \"utf-8\"; \
		substitute = \"?\";\n \
		force_substitute = true>\n"
);;

(* report *)

Printf.eprintf "ok\n";;
