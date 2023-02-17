(* #load "iconv.cma";; *)
open Iconv;;
open Iconv_pp;;
let c = iconv_open ~tocode:"sjis" ~fromcode:"utf-8";;
Format.printf "%a@." pp_iconv c;;
