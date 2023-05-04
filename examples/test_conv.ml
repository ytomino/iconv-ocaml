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

let c = iconv_open ~tocode:"ISO-2022-JP" ~fromcode:"UTF-8" in
let s = "Aあ" in
let x = iconv c s in
if verbose then print_endline (String.escaped x);
assert (x = "\x41\x1B\x24\x42\x24\x22\x1B\x28\x42");;

(* out_iconv *)

let buf = Buffer.create 256 in
let w = Iconv.open_out ~tocode:"ISO-2022-JP" ~fromcode:"UTF-16BE"
	(Buffer.add_substring buf)
in
let s = "\x00\x41\x30\x42" in
for i = 0 to String.length s - 1 do
	Iconv.output_substring w s i 1
done;
Iconv.end_out w;
let x = Buffer.contents buf in
if verbose then print_endline (String.escaped x);
assert (x = "\x41\x1B\x24\x42\x24\x22\x1B\x28\x42");;

(* report *)

Printf.eprintf "ok\n";;
