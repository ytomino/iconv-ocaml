external iconv_get_version_opt: unit -> (int * int) option =
	"mliconv_get_version_opt"
(** Return [Some (minor, major)] if GNU libiconv is used, otherswise [None]. *)

val iconv_get_version_string_opt: unit -> string option

type iconv_t

external iconv_open: tocode:string -> fromcode:string -> iconv_t =
	"mliconv_open"

external substitute: iconv_t -> string = "mliconv_substitute"
external set_substitute: iconv_t -> string -> unit = "mliconv_set_substitute"

external unexist: iconv_t -> [> `auto | `illegal_sequence] =
	"mliconv_unexist"
external set_unexist: iconv_t -> [< `auto | `illegal_sequence] -> unit =
	"mliconv_set_unexist"
(** Correspond to [ICONV_SET_ILSEQ_INVALID] of Citrus.
    Citrus iconv internally substitutes valid characters does not exist in
    [tocode] if [`auto].
    It is always [`illegal_sequence] in GNU libiconv or glibc. *)

external min_sequence_in_fromcode: iconv_t -> int =
	"mliconv_min_sequence_in_fromcode"

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

type iconv_decode_state
type iconv_decode = private iconv_t * iconv_decode_state

val iconv_open_decode: fromcode:string -> iconv_decode

type iconv_decode_error = [`illegal_sequence | `none | `truncated]

val iconv_decode: iconv_decode -> ('a -> 'b -> char) -> ('a -> 'b -> 'b) ->
	('a -> 'b -> bool) -> ('a -> 'b -> 'b -> Uchar.t -> 'c) ->
	fail:('a -> 'b -> 'b -> [> iconv_decode_error] -> 'c) -> 'a -> 'b -> 'c

module Out_iconv = Iconv__Out_iconv
