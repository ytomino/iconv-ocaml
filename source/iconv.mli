external iconv_get_version_opt: unit -> (int * int) option =
	"mliconv_get_version_opt"
(** Return [Some (minor, major)] if GNU libiconv is used, otherswise [None]. *)

val iconv_get_version_string_opt: unit -> string option

type iconv_t

external iconv_open: tocode:string -> fromcode:string -> iconv_t =
	"mliconv_open"
external iconv_string: iconv_t -> string -> string = "mliconv_string"

external substitute: iconv_t -> string = "mliconv_substitute"
external set_substitute: iconv_t -> string -> unit = "mliconv_set_substitute"

external force_substitute: iconv_t -> bool = "mliconv_force_substitute"
external set_force_substitute: iconv_t -> bool -> unit =
	"mliconv_set_force_substitute"
(** Correspond to [ICONV_SET_ILSEQ_INVALID] of Citrus.
    Citrus iconv internally substitutes valid characters does not exist in
    [tocode] if [false].
    It is always [true] in GNU libiconv or glibc. *)

type out_state
type out_iconv = iconv_t * out_state

val open_out: tocode:string -> fromcode:string ->
	(string -> int -> int -> unit) -> out_iconv
val output_substring: out_iconv -> string -> int -> int -> unit
val output_string: out_iconv -> string -> unit
val flush: out_iconv -> unit
val end_out: out_iconv -> unit
val reset_out: out_iconv -> unit
