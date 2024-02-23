open Iconv;;

external tocode: iconv_t -> string = "mliconv_tocode";;
external fromcode: iconv_t -> string = "mliconv_fromcode";;

let pp_iconv (f: Format.formatter) (conv: iconv_t) = (
	Format.fprintf f
		"<@[\
			tocode = \"%s\";@ \
			fromcode = \"%s\";@ \
			substitute = \"%s\";@ \
			force_substitute = %b\
		@]>"
		(String.escaped (tocode conv)) (String.escaped (fromcode conv))
		(String.escaped (substitute conv)) (force_substitute conv)
);;
