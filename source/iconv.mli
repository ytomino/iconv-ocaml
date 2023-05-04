external iconv_get_version_opt: unit -> (int * int) option =
	"mliconv_get_version_opt"
(** Return [Some (minor, major)] if GNU libiconv is used, otherswise [None]. *)

val iconv_get_version_string_opt: unit -> string option

type iconv_t

external iconv_open: tocode:string -> fromcode:string -> iconv_t =
	"mliconv_open"
external iconv: iconv_t -> string -> string = "mliconv_convert"

external substitute: iconv_t -> string = "mliconv_substitute"
external set_substitute: iconv_t -> string -> unit = "mliconv_set_substitute"

type out_state
type out_iconv = iconv_t * out_state

val open_out: tocode:string -> fromcode:string ->
	(string -> int -> int -> unit) -> out_iconv
val output_substring: out_iconv -> string -> int -> int -> unit
val flush: out_iconv -> unit
val end_out: out_iconv -> unit
val reset_out: out_iconv -> unit
