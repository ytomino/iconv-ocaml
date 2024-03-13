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

external force_substitute: iconv_t -> bool = "mliconv_force_substitute";;
external set_force_substitute: iconv_t -> bool -> unit =
	"mliconv_set_force_substitute";;

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

type iconv_fields = {
	mutable inbuf: string;
	mutable inbuf_offset: int;
	mutable inbytesleft: int;
	mutable outbuf: bytes;
	mutable outbuf_offset: int;
	mutable outbytesleft: int
};;

external iconv: iconv_t -> iconv_fields -> bool -> bool = "mliconv_iconv";;
external iconv_end: iconv_t -> iconv_fields -> bool = "mliconv_iconv_end";;
external iconv_reset: iconv_t -> unit = "mliconv_iconv_reset";;

type out_state = iconv_fields * (string -> int -> int -> unit);;
type out_iconv = iconv_t * out_state;;

let outbuf_capacity = 240;;

let open_out ~(tocode: string) ~(fromcode: string)
	(f: string -> int -> int -> unit) =
(
	let cd = iconv_open ~tocode ~fromcode in
	cd, (
		{
			inbuf = "";
			inbuf_offset = 0;
			inbytesleft = 0;
			outbuf = Bytes.create outbuf_capacity;
			outbuf_offset = 0;
			outbytesleft = outbuf_capacity;
		}, f
	)
);;

let do_flush (fields, f: out_state) = (
		let out_length = fields.outbuf_offset in
		if out_length > 0 then (
			f (Bytes.unsafe_to_string fields.outbuf) 0 out_length;
			let outbuf_length = Bytes.length fields.outbuf in
			fields.outbuf_offset <- 0;
			fields.outbuf <- Bytes.create outbuf_length;
			fields.outbytesleft <- outbuf_length
		)
);;

let unsafe_output_substring: out_iconv -> string -> int -> int -> unit =
	let rec loop oi = (
		let cd, state = oi in
		let fields, _ = state in
		if iconv cd fields false then ()
		else (
			do_flush state;
			loop oi
		)
	) in
	fun oi s offset len ->
	let _, (fields, _) = oi in
	if fields.inbytesleft = 0 then (
		fields.inbuf <- s;
		fields.inbuf_offset <- offset;
		fields.inbytesleft <- len
	) else (
		let inbuf_length = fields.inbytesleft + len in
		let inbuf = Bytes.create inbuf_length in
		Bytes.blit_string fields.inbuf fields.inbuf_offset inbuf 0 fields.inbytesleft;
		Bytes.blit_string s offset inbuf fields.inbytesleft len;
		fields.inbuf <- Bytes.unsafe_to_string inbuf;
		fields.inbuf_offset <- 0;
		fields.inbytesleft <- inbuf_length
	);
	loop oi;;

let output_substring (oi: out_iconv) (s: string) (offset: int) (len: int) = (
	if offset >= 0 && len >= 0 && offset + len <= String.length s
	then unsafe_output_substring oi s offset len
	else invalid_arg "Iconv.output_substring" (* __FUNCTION__ *)
);;

let output_string (oi: out_iconv) (s: string) = (
	unsafe_output_substring oi s 0 (String.length s)
);;

let flush (_, state: out_iconv) = (
	do_flush state
);;

let end_out (cd, state: out_iconv) = (
	let loc = "Iconv.end_out" (* __FUNCTION__ *) in
	let fields, f = state in
	if fields.inbytesleft > 0 && not (iconv cd fields true) then (
		do_flush state;
		if not (iconv cd fields true) then failwith loc
	);
	assert (fields.inbytesleft = 0);
	if not (iconv_end cd fields) then (
		do_flush state;
		if not (iconv_end cd fields) then failwith loc
	);
	(* No need to reset the output buffer for reuse. *)
	let out_length = fields.outbuf_offset in
	if out_length > 0 then (
		f (Bytes.unsafe_to_string fields.outbuf) 0 out_length;
		fields.outbuf <- Bytes.empty;
		fields.outbytesleft <- 0
	)
);;

let reset_out (cd, state: out_iconv) = (
	iconv_reset cd;
	let fields, _ = state in
	fields.inbytesleft <- 0;
	(* Restore from the ended state. *)
	fields.outbuf_offset <- 0;
	if Bytes.length fields.outbuf <> outbuf_capacity then (
		fields.outbuf <- Bytes.create outbuf_capacity
	);
	fields.outbytesleft <- outbuf_capacity
);;
