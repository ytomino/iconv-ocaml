open Iconv;;

type out_state = iconv_fields * (string -> int -> int -> unit);;
type t = iconv_t * out_state;;

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

let unsafe_output_substring: t -> string -> int -> int -> unit =
	let rec loop oi = (
		let cd, state = oi in
		let fields, _ = state in
		match iconv_substitute cd fields false with
		| `ok ->
			()
		| `overflow ->
			do_flush state;
			loop oi
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

let output_substring (oi: t) (s: string) (offset: int) (len: int) = (
	if offset >= 0 && len >= 0 && len <= String.length s - offset
	then unsafe_output_substring oi s offset len
	else invalid_arg "Iconv.output_substring" (* __FUNCTION__ *)
);;

let output_string (oi: t) (s: string) = (
	unsafe_output_substring oi s 0 (String.length s)
);;

let flush (_, state: t) = (
	do_flush state
);;

let end_out (cd, state: t) = (
	let loc = "Iconv.end_out" (* __FUNCTION__ *) in
	let fields, f = state in
	if fields.inbytesleft > 0 then (
		match iconv_substitute cd fields true with
		| `ok ->
			()
		| `overflow ->
			do_flush state;
			match iconv_substitute cd fields true with
			| `ok ->
				()
			| `overflow ->
				failwith loc
	);
	assert (fields.inbytesleft = 0);
	begin match iconv_end cd fields with
	| `ok ->
		()
	| `overflow ->
		do_flush state;
		match iconv_end cd fields with
		| `ok ->
			()
		| `overflow ->
			failwith loc
	end;
	(* No need to reset the output buffer for reuse. *)
	let out_length = fields.outbuf_offset in
	if out_length > 0 then (
		f (Bytes.unsafe_to_string fields.outbuf) 0 out_length;
		fields.outbuf <- Bytes.empty;
		fields.outbytesleft <- 0
	)
);;

let reset_out (cd, state: t) = (
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
