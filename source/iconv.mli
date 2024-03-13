external iconv_get_version_opt: unit -> (int * int) option =
	"mliconv_get_version_opt"
(** Return [Some (minor, major)] if GNU libiconv is used, otherswise [None]. *)

val iconv_get_version_string_opt: unit -> string option

type iconv_t

external iconv_open: tocode:string -> fromcode:string -> iconv_t =
	"mliconv_open"

external substitute: iconv_t -> string = "mliconv_substitute"
external set_substitute: iconv_t -> string -> unit = "mliconv_set_substitute"

external force_substitute: iconv_t -> bool = "mliconv_force_substitute"
external set_force_substitute: iconv_t -> bool -> unit =
	"mliconv_set_force_substitute"
(** Correspond to [ICONV_SET_ILSEQ_INVALID] of Citrus.
    Citrus iconv internally substitutes valid characters does not exist in
    [tocode] if [false].
    It is always [true] in GNU libiconv or glibc. *)

type iconv_fields = {
	mutable inbuf: string;
	mutable inbuf_offset: int;
	mutable inbytesleft: int;
	mutable outbuf: bytes;
	mutable outbuf_offset: int;
	mutable outbytesleft: int
}

val iconv: iconv_t -> iconv_fields -> bool ->
	[> `ok | `overflow | `illegal_sequence]
val iconv_substitute: iconv_t -> iconv_fields -> bool -> [> `ok | `overflow]
val iconv_end: iconv_t -> iconv_fields -> [> `ok | `overflow]
external iconv_reset: iconv_t -> unit = "mliconv_iconv_reset"

val iconv_substring: iconv_t -> string -> int -> int -> string
val iconv_string: iconv_t -> string -> string

module Out_iconv = Iconv__Out_iconv
