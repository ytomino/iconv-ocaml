let f fmt = Lib_test.f __FILE__ fmt;;
let fa a = Lib_test.fa __FILE__ a;;

let output_uchar oc x = (
	Printf.fprintf oc "U+%.4x" (Uchar.to_int x)
);;

open Iconv;;

let c = iconv_open ~tocode:"sjis" ~fromcode:"utf-8" in
let s = "ソースコード直書きのUTF-8文字列です\n" in
let x = iconv_string c s |> f __LINE__ "%S" in
assert (
	x = "\131\\\129[\131X\131R\129[\131h\146\188\143\145\130\171\130\204UTF-8"
		^ "\149\182\142\154\151\241\130\197\130\183\n"
);
assert (iconv_substring c s 0 (String.length s) = x);
assert (iconv_substring c s 0 3 = "\131\\");
assert (iconv_substring c s (String.length s - 1) 1 = "\n");
assert (iconv_substring c s 0 0 = "");
assert (iconv_substring c s (String.length s) 0 = "");;

let c = iconv_open ~tocode:"ISO-2022-JP" ~fromcode:"ISO-8859-1" in
let s = "\xA2" in
let x = iconv_string c s |> f __LINE__ "%S" in
assert (x = "\x1B\x24\x42\x21\x71\x1B\x28\x42");;

let c = iconv_open ~tocode:"ISO-2022-JP" ~fromcode:"UTF-8" in
let s = "Aあ" in
let x = iconv_string c s |> f __LINE__ "%S" in
assert (x = "\x41\x1B\x24\x42\x24\x22\x1B\x28\x42");;

(* decode *)

let d = iconv_open_decode ~fromcode:"EUC-JP" in
set_unexist (fst (d :> iconv_t * iconv_decode_state)) `illegal_sequence;
let c: Uchar.t =
	iconv_decode d String.get (fun _ -> succ) (fun s i -> String.length s <= i)
		(fun _ start_pos end_pos c ->
			assert (
				start_pos |> f __LINE__ "%d" = 0
				&& end_pos |> f __LINE__ "%d" = 1
			);
			c |> fa __LINE__ output_uchar
		)
		~fail:(fun _ _ _ _ -> assert false) "A$" 0
in
assert (Uchar.to_int c = Char.code 'A');
let c: Uchar.t =
	iconv_decode d String.get (fun _ -> succ) (fun s i -> String.length s <= i)
		(fun _ start_pos end_pos c ->
			assert (
				start_pos |> f __LINE__ "%d" = 0
				&& end_pos |> f __LINE__ "%d" = 2
			);
			c |> fa __LINE__ output_uchar
		)
		~fail:(fun _ _ _ _ -> assert false) "\xa3\xc1$" 0
in
assert (Uchar.to_int c = 0xff21);
let () =
	iconv_decode d String.get (fun _ -> succ) (fun s i -> String.length s <= i)
		(fun _ _ _ -> assert false)
		~fail:(fun _ start_pos end_pos error ->
			match error with
			| `truncated ->
				assert (
					start_pos |> f __LINE__ "%d" = 0
					&& end_pos |> f __LINE__ "%d" = 1
				)
			| `illegal_sequence | `none ->
				assert false
		)
		"\xa3" 0
in
let () =
	iconv_decode d String.get (fun _ -> succ) (fun s i -> String.length s <= i)
		(fun _ _ _ -> assert false)
		~fail:(fun _ start_pos end_pos error ->
			match error with
			| `illegal_sequence ->
				assert (
					start_pos |> f __LINE__ "%d" = 0
					&& end_pos |> f __LINE__ "%d" = 1
				)
			| `none | `truncated ->
				assert false
		)
		"\xa3\x00" 0
in
();;

let d = iconv_open_decode ~fromcode:"ISO-2022-JP" in
set_unexist (fst (d :> iconv_t * iconv_decode_state)) `illegal_sequence;
(* The output is none but it switches to JIS X 0208 by escape sequence. *)
let () =
	iconv_decode d String.get (fun _ -> succ) (fun s i -> String.length s <= i)
		(fun _ _ _ -> assert false)
		~fail:(fun _ start_pos end_pos error ->
			match error with
			| `none ->
				assert (
					start_pos |> f __LINE__ "%d" = 0
					&& end_pos |> f __LINE__ "%d" = 3
				)
			| `illegal_sequence | `truncated ->
				assert false
		)
		"\x1b\x24\x42" 0
in
(* In JIS X 0208. *)
let c: Uchar.t =
	iconv_decode d String.get (fun _ -> succ) (fun s i -> String.length s <= i)
		(fun _ start_pos end_pos c ->
			assert (
				start_pos |> f __LINE__ "%d" = 0
				&& end_pos |> f __LINE__ "%d" = 2);
			c |> fa __LINE__ output_uchar
		)
		~fail:(fun _ _ _ _ -> assert false)
		"\x23\x41" 0
in
assert (Uchar.to_int c = 0xff21);
(* From multi-bytes to multi-codepoints. *)
let rest: int =
	iconv_decode d String.get (fun _ -> succ) (fun s i -> String.length s <= i)
		(fun _ start_pos end_pos c ->
			assert (
				start_pos |> f __LINE__ "%d" = 0
				&& (
					end_pos |> f __LINE__ "%d" = 2 (* Citrus *)
					|| end_pos = 3 (* glibc *)
				)
			);
			assert (c |> fa __LINE__ output_uchar = Uchar.of_int 0x1b);
			end_pos - 1 (* behind of "\x1b" is/are queued *)
		)
		~fail:(fun _ start_pos end_pos error ->
			match error with
			| `illegal_sequence ->
				assert (
					start_pos |> f __LINE__ "%d" = 0
					&& end_pos |> f __LINE__ "%d" = 1 (* GNU libiconv *)
				);
				0 (* no queued *)
			| `none | `truncated ->
				assert false
		)
		"\x1b\x01\x02" 0
in
for i = 1 to rest do
	let c: Uchar.t =
		iconv_decode d String.get (fun _ -> succ) (fun s i -> String.length s <= i)
			(fun _ start_pos end_pos c ->
				assert (
					start_pos |> f __LINE__ "%d" = 0
					&& end_pos |> f __LINE__ "%d" = 0
				);
				c |> fa __LINE__ output_uchar
			)
			~fail:(fun _ _ _ _ -> assert false)
			"" 0 (* use the rest of previous conversion  *)
	in
	assert (Uchar.to_int c = i)
done;
(* Detect truncated, and end of the revious sequence. *)
let () =
	iconv_decode d String.get (fun _ -> succ) (fun s i -> String.length s <= i)
		(fun _ _ _ -> assert false)
		~fail:(fun _ start_pos end_pos error ->
			match error with
			| `truncated ->
				assert (
					start_pos |> f __LINE__ "%d" = 0
					&& end_pos |> f __LINE__ "%d" = 1
				)
			| `illegal_sequence | `none ->
				assert false
		)
		"\x24" 0
in
(* Confirm the normal state in JIS X 0208. *)
let c: Uchar.t =
	iconv_decode d String.get (fun _ -> succ) (fun s i -> String.length s <= i)
		(fun _ start_pos end_pos c ->
			assert (
				start_pos |> f __LINE__ "%d" = 0
				&& end_pos |> f __LINE__ "%d" = 2
			);
			c |> fa __LINE__ output_uchar
		)
		~fail:(fun _ _ _ _ -> assert false)
		"\x24\x22" 0
in
assert (Uchar.to_int c = 0x3042);;

(* out_iconv *)

let buf = Buffer.create 256 in
let w =
	Out_iconv.open_out ~tocode:"ISO-2022-JP" ~fromcode:"UTF-16BE"
		(Buffer.add_substring buf)
in
set_unexist (fst (w :> iconv_t * Out_iconv.out_state)) `illegal_sequence;
let s = "\x00\x41\x30\x42\x00\xA0" in (* U+00A0 will be substituted *)
for i = 0 to String.length s - 1 do
	Out_iconv.output_substring w s i 1
done;
Out_iconv.end_out w;
let x = Buffer.contents buf |> f __LINE__ "%S" in
assert (x = "\x41\x1B\x24\x42\x24\x22\x1B\x28\x42\x3F");;

(* report *)

prerr_endline "ok";;
