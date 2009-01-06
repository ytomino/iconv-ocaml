open Iconv;;

external iconv_tocode: iconv_t -> string = "mliconv_tocode";;
external iconv_fromcode: iconv_t -> string = "mliconv_fromcode";;

let pp_iconv (f: Format.formatter) (conv: iconv_t): unit = (
	Format.fprintf f "<to %s from %s>" (iconv_tocode conv) (iconv_fromcode conv)
);;
