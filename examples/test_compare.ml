(* If it was built without SUPPORT_COMPARISON,
   Fatal error: exception Invalid_argument("compare: abstract value") *)

let x = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"LATIN1" in
let y = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"LATIN1" in
assert (x = y);
assert (Hashtbl.hash x = Hashtbl.hash y);;

let x = Iconv.iconv_open ~tocode:"LATIN1" ~fromcode:"ASCII" in
let y = Iconv.iconv_open ~tocode:"ASCII" ~fromcode:"LATIN1" in
assert (x <> y);
assert (Hashtbl.hash x <> Hashtbl.hash y);;

(* report *)

Printf.eprintf "ok\n";;
