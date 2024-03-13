open Iconv

type out_state
type t = iconv_t * out_state

val open_out: tocode:string -> fromcode:string ->
	(string -> int -> int -> unit) -> t
val output_substring: t -> string -> int -> int -> unit
val output_string: t -> string -> unit
val flush: t -> unit
val end_out: t -> unit
val reset_out: t -> unit
