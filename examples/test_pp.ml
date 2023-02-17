let verbose = Array.length Sys.argv > 1 && Sys.argv.(1) = "--verbose";;

open Iconv;;
open Iconv_pp;;
let c = iconv_open ~tocode:"sjis" ~fromcode:"utf-8";;
let x = Format.asprintf "%a@." pp_iconv c in
if verbose then Printf.printf "%s\n" (String.escaped x);
assert (x = "<to SHIFT_JIS from UTF-8>\n" || x = "<to sjis from utf-8>\n");;

(* report *)

Printf.eprintf "ok\n";;
