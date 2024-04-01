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

external min_sequence_in_fromcode: iconv_t -> int =
	"mliconv_min_sequence_in_fromcode";;

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
	&& inbytesleft <= String.length inbuf - inbuf_offset
);;

let valid_out (fields: iconv_fields) = (
	let {outbuf; outbuf_offset; outbytesleft; _} = fields in
	outbuf_offset >= 0 && outbytesleft >= 0
	&& outbytesleft <= Bytes.length outbuf - outbuf_offset
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
	if pos >= 0 && len >= 0 && len <= String.length s - pos
	then unsafe_iconv_substring cd s pos len
	else invalid_arg "Iconv.iconv_substring" (* __FUNCTION__ *)
);;

let iconv_string (cd: iconv_t) (s: string) = (
	unsafe_iconv_substring cd s 0 (String.length s)
);;

let decode_inbuf_offset = 0;;
let decode_inbuf_capacity = 16;; (* greater than MAX_SEQUENCE + 1 *)
let decode_outbuf_offset = decode_inbuf_capacity;;
let decode_outbuf_capacity = 16;;
	(* greater than 12 as 3(ISO/IEC 2022 escape sequence) * 4(UTF-32) *)

let get_uchar (s: bytes) (pos: int) loc = (
	let code = Bytes.get_int32_be s pos in
	match Int32.unsigned_to_int code with
	| None -> failwith loc
	| Some code -> Uchar.unsafe_of_int code
);;

type iconv_decode_state = iconv_fields;;
type iconv_decode = iconv_t * iconv_decode_state;;

let iconv_open_decode ~(fromcode: string) = (
	let cd = iconv_open ~tocode:"UTF-32BE" ~fromcode in
	let buf = Bytes.create (decode_inbuf_capacity + decode_outbuf_capacity) in
	cd, {
		inbuf = Bytes.unsafe_to_string buf;
		inbuf_offset = decode_inbuf_offset;
		inbytesleft = 0;
		outbuf = buf;
		outbuf_offset = decode_outbuf_offset;
		outbytesleft = decode_outbuf_capacity
	}
);;

type iconv_decode_error = [`illegal_sequence | `none | `truncated]

let iconv_decode: type a b c. iconv_decode -> (a -> b -> char) ->
	(a -> b -> b) -> (a -> b -> bool) -> (a -> b -> b -> Uchar.t -> c) ->
	fail:(a -> b -> b -> [> iconv_decode_error] -> c) -> a -> b -> c =
	let loc = "Iconv.iconv_decode" (* __FUNCTION__ *) in
	let reset_in_fields fields = (
		fields.inbuf_offset <- decode_inbuf_offset;
		fields.inbytesleft <- 0
	) in
	let reset_out_fields fields = (
		fields.outbuf_offset <- decode_outbuf_offset;
		fields.outbytesleft <- decode_outbuf_capacity
	) in
	let rec nth tl_f a b n = (
		if n = 0 then b
		else nth tl_f a (tl_f a b) (n - 1)
	) in
	let decode_out decode cont_f a b b' = (
		let _, fields = decode in
		let {outbuf; outbuf_offset; outbytesleft; _} = fields in
		let outbuf_used = outbuf_offset - decode_outbuf_offset in
		if outbuf_used mod 4 = 0 then (
			assert (outbuf_used >= 4);
			let item = get_uchar outbuf decode_outbuf_offset loc in
			if outbuf_used = 4 then reset_out_fields fields
			else (
				Bytes.blit outbuf (decode_outbuf_offset + 4) outbuf decode_outbuf_offset
					(outbuf_used - 4);
				fields.outbuf_offset <- outbuf_offset - 4;
				fields.outbytesleft <- outbytesleft + 4;
			);
			cont_f a b b' item
		) else failwith loc (* output is not UTF-32 *)
	) in
	let rec decode_in decode hd_f tl_f is_empty_f cont_f fail a b b' = (
		let elt = hd_f a b' in
		let b' = tl_f a b' in
		let cd, fields = decode in
		assert (
			fields.inbuf_offset = decode_inbuf_offset
			&& fields.inbytesleft >= 0 && fields.inbytesleft <= decode_inbuf_capacity
			&& fields.outbuf_offset = decode_outbuf_offset
			&& fields.outbytesleft = decode_outbuf_capacity
		);
		let {inbytesleft; outbuf; _} = fields in
		Bytes.set outbuf inbytesleft elt;
		let inbytesleft = inbytesleft + 1 in
		fields.inbytesleft <- inbytesleft;
		match iconv cd fields false with
		| `ok ->
			let {inbytesleft = inbytesleft'; _} = fields in
			if fields.outbuf_offset > decode_outbuf_offset then (
				let end_pos =
					if inbytesleft' = 0 then b'
					else nth tl_f a b (inbytesleft - inbytesleft')
				in
				decode_out decode cont_f a b end_pos
			) else if not (is_empty_f a b') then (
				decode_in decode hd_f tl_f is_empty_f cont_f fail a b b'
			) else if inbytesleft' = inbytesleft then (
				reset_out_fields fields;
				fail a b b' `truncated
			) else (
				let end_pos =
					if inbytesleft' = 0 then b'
					else nth tl_f a b (inbytesleft - inbytesleft')
				in
				fail a b end_pos `none
			)
		| `overflow ->
			failwith loc (* decode_outbuf_capacity is smaller *)
		| `illegal_sequence ->
			let end_pos =
				let min_sequence = min_sequence_in_fromcode cd in
				if min_sequence >= inbytesleft then b'
				else nth tl_f a b min_sequence
			in
			fail a b end_pos `illegal_sequence
	) in
	fun decode hd_f tl_f is_empty_f cont_f ~fail a b ->
	let _, fields = decode in
	if fields.outbuf_offset = decode_outbuf_offset then (
		reset_in_fields fields;
		decode_in decode hd_f tl_f is_empty_f cont_f fail a b b
	) else decode_out decode cont_f a b b;;

module Out_iconv = Iconv__Out_iconv;;
