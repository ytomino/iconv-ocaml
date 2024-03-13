let f fmt = Lib_test.f __FILE__ fmt;;

open Iconv;;
open Iconv_pp;;
let c = iconv_open ~tocode:"sjis" ~fromcode:"utf-8";;
set_unexist c `illegal_sequence;
let x = Format.asprintf "%a@." pp_iconv c |> f __LINE__ "%S" in
assert (
	x = "<\
		tocode = \"SHIFT_JIS\"; \
		fromcode = \"UTF-8\"; \
		substitute = \"?\";\n \
		unexist = `illegal_sequence>\n"
	|| x = "<\
		tocode = \"sjis\"; \
		fromcode = \"utf-8\"; \
		substitute = \"?\";\n \
		unexist = `illegal_sequence>\n"
);;

(* report *)

prerr_endline "ok";;
