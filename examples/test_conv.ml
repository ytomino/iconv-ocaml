(* #load "iconv.cma";; *)
open Iconv;;

let c = iconv_open ~tocode:"sjis" ~fromcode:"utf-8";;
let s = "ソースコード直書きのUTF-8文字列です\n" in
print_string (iconv c s);;
