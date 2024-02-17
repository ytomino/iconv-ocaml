open Iconv;;

external tocode: iconv_t -> string = "mliconv_tocode";;
external fromcode: iconv_t -> string = "mliconv_fromcode";;

let pp_iconv (f: Format.formatter) (conv: iconv_t): unit = (
	Format.fprintf f "<to %s from %s>" (tocode conv) (fromcode conv)
);;
