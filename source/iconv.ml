external iconv_get_version_opt: unit -> (int * int) option =
	"mliconv_get_version_opt";;

let iconv_get_version_string_opt () = (
	match iconv_get_version_opt () with
	| Some (major, minor) -> Some (string_of_int major ^ "." ^ string_of_int minor)
	| None -> None
);;

type iconv_t;;

external iconv_open: tocode:string -> fromcode:string -> iconv_t =
	"mliconv_open";;

external substitute: iconv_t -> string = "mliconv_substitute";;
external set_substitute: iconv_t -> string -> unit = "mliconv_set_substitute";;

external unexist: iconv_t -> [> `auto | `illegal_sequence] =
	"mliconv_unexist";;
external set_unexist: iconv_t -> [< `auto | `illegal_sequence] -> unit =
	"mliconv_set_unexist";;

type iconv_fields = {
	mutable inbuf: string;
	mutable inbuf_offset: int;
	mutable inbytesleft: int;
	mutable outbuf: bytes;
	mutable outbuf_offset: int;
	mutable outbytesleft: int
};;

let valid_in (fields: iconv_fields) = (
	let {inbuf; inbuf_offset; inbytesleft; _} = fields in
	inbuf_offset >= 0 && inbytesleft >= 0
	&& inbuf_offset + inbytesleft <= String.length inbuf
);;

let valid_out (fields: iconv_fields) = (
	let {outbuf; outbuf_offset; outbytesleft; _} = fields in
	outbuf_offset >= 0 && outbytesleft >= 0
	&& outbuf_offset + outbytesleft <= Bytes.length outbuf
);;

external unsafe_iconv: iconv_t -> iconv_fields -> bool ->
	[> `ok | `overflow | `illegal_sequence] =
	"mliconv_unsafe_iconv";;

let iconv (cd: iconv_t) (fields: iconv_fields) (finish: bool) = (
	if valid_in fields && valid_out fields then unsafe_iconv cd fields finish
	else invalid_arg "Iconv.iconv" (* __FUNCTION__ *)
);;

external unsafe_iconv_substitute: iconv_t -> iconv_fields -> bool ->
	[> `ok | `overflow] =
	"mliconv_unsafe_iconv_substitute";;

let iconv_substitute (cd: iconv_t) (fields: iconv_fields) (finish: bool) = (
	if valid_in fields && valid_out fields
	then unsafe_iconv_substitute cd fields finish
	else invalid_arg "Iconv.iconv_substitute" (* __FUNCTION__ *)
);;

external unsafe_iconv_end: iconv_t -> iconv_fields -> [> `ok | `overflow] =
	"mliconv_unsafe_iconv_end";;

let iconv_end (cd: iconv_t) (fields: iconv_fields) = (
	if valid_out fields then unsafe_iconv_end cd fields
	else invalid_arg "Iconv.iconv_end" (* __FUNCTION__ *)
);;

external iconv_reset: iconv_t -> unit = "mliconv_iconv_reset";;

external unsafe_iconv_substring: iconv_t -> string -> int -> int -> string =
	"mliconv_unsafe_iconv_substring";;

let iconv_substring (cd: iconv_t) (s: string) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && pos + len <= String.length s
	then unsafe_iconv_substring cd s pos len
	else invalid_arg "Iconv.iconv_substring" (* __FUNCTION__ *)
);;

let iconv_string (cd: iconv_t) (s: string) = (
	unsafe_iconv_substring cd s 0 (String.length s)
);;

module Out_iconv = Iconv__Out_iconv;;
