(* #load "iconv.cma";; *)
open Iconv;;
open Iconv_pp;;
let c = iconv_open ~tocode:"sjis" ~fromcode:"utf-8";;
Format.printf "%a@." pp_iconv c;;
let s = "ソースコード直書きのUTF-8文字列です\n" in
print_string (iconv c s);;
