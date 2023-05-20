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
external iconv: iconv_t -> string -> string = "mliconv_convert";;

external substitute: iconv_t -> string = "mliconv_substitute";;
external set_substitute: iconv_t -> string -> unit = "mliconv_set_substitute";;

external force_substitute: iconv_t -> bool = "mliconv_force_substitute";;
external set_force_substitute: iconv_t -> bool -> unit =
	"mliconv_set_force_substitute";;

type out_state = {
	mutable inbuf: string;
	mutable inbuf_offset: int;
	mutable inbytesleft: int;
	outbuf: bytes;
	mutable outbuf_offset: int;
	mutable outbytesleft: int;
	f: (string -> int -> int -> unit)
};;

type out_iconv = iconv_t * out_state;;

external iconv_step: iconv_t -> out_state -> bool -> bool = "mliconv_iconv";;
external iconv_end: iconv_t -> out_state -> bool = "mliconv_iconv_end";;
external iconv_reset: iconv_t -> unit = "mliconv_iconv_reset";;

let open_out ~(tocode: string) ~(fromcode: string)
	(f: string -> int -> int -> unit) =
(
	let cd = iconv_open ~tocode ~fromcode in
	let outbuf_length = 240 in
	cd, {
		inbuf = "";
		inbuf_offset = 0;
		inbytesleft = 0;
		outbuf = Bytes.create outbuf_length;
		outbuf_offset = 0;
		outbytesleft = outbuf_length;
		f
	}
);;

let do_flush (state: out_state) = (
		let out_length = state.outbuf_offset in
		if out_length > 0 then (
			state.outbuf_offset <- 0;
			state.outbytesleft <- Bytes.length state.outbuf;
			state.f (Bytes.unsafe_to_string state.outbuf) 0 out_length
		)
);;

let output_substring (cd, state: out_iconv) (s: string) (offset: int)
	(len: int) =
(
	if len <= 0 then () else
	if offset < 0 || offset + len > String.length s then (
		invalid_arg "Iconv.output_string"
	);
	if state.inbytesleft = 0 then (
		state.inbuf <- s;
		state.inbuf_offset <- offset;
		state.inbytesleft <- len
	) else (
		let inbuf_length = state.inbytesleft + len in
		let inbuf = Bytes.create inbuf_length in
		Bytes.blit_string state.inbuf state.inbuf_offset inbuf 0 state.inbytesleft;
		Bytes.blit_string s offset inbuf state.inbytesleft len;
		state.inbuf <- Bytes.unsafe_to_string inbuf;
		state.inbuf_offset <- 0;
		state.inbytesleft <- inbuf_length
	);
	let rec loop () = (
		if iconv_step cd state false then ()
		else (
			do_flush state;
			loop ()
		)
	) in
	loop ()
);;

let flush (_, state: out_iconv) = (
	do_flush state
);;

let end_out (cd, state: out_iconv) = (
	if state.inbytesleft > 0 && not (iconv_step cd state true) then (
		do_flush state;
		if not (iconv_step cd state true) then failwith "Iconv.end_out"
	);
	if not (iconv_end cd state) then (
		do_flush state;
		if not (iconv_end cd state) then failwith "Iconv.end_out"
	);
	do_flush state
);;

let reset_out (cd, state: out_iconv) = (
	iconv_reset cd;
	state.inbuf_offset <- 0;
	state.inbytesleft <- 0;
	state.outbuf_offset <- 0;
	state.outbytesleft <- Bytes.length state.outbuf
);;
