open Iconv

type out_state
type out_iconv = iconv_t * out_state

val open_out: tocode:string -> fromcode:string ->
	(string -> int -> int -> unit) -> out_iconv
val output_substring: out_iconv -> string -> int -> int -> unit
val output_string: out_iconv -> string -> unit
val flush: out_iconv -> unit
val end_out: out_iconv -> unit
val reset_out: out_iconv -> unit
