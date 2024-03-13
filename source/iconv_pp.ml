open Iconv;;

external tocode: iconv_t -> string = "mliconv_tocode";;
external fromcode: iconv_t -> string = "mliconv_fromcode";;

let string_of_unexist (x: [< `auto | `illegal_sequence]) = (
	match x with
	| `auto -> "`auto"
	| `illegal_sequence -> "`illegal_sequence"
);;

let pp_iconv (f: Format.formatter) (conv: iconv_t) = (
	Format.fprintf f
		"<@[\
			tocode = \"%s\";@ \
			fromcode = \"%s\";@ \
			substitute = \"%s\";@ \
			unexist = %s\
		@]>"
		(String.escaped (tocode conv)) (String.escaped (fromcode conv))
		(String.escaped (substitute conv)) (string_of_unexist (unexist conv))
);;
