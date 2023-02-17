type iconv_t

val iconv_open: tocode:string -> fromcode:string -> iconv_t
val iconv: iconv_t -> string -> string
