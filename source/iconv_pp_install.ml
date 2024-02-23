module Longident = struct
	type t =
		| Lident of string
		| Ldot of t * string
		| Lapply of t * t;;
end;;
open Longident;;
Topdirs.dir_install_printer Format.std_formatter
	(Obj.magic (Ldot (Lident "Iconv_pp", "pp_iconv")));;
