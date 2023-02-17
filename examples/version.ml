match Iconv.iconv_get_version_string_opt () with
| Some version ->
	print_string version;
	print_newline ()
| None ->
	prerr_string Sys.argv.(0);
	prerr_string ": iconv is not GNU libiconv";
	prerr_newline ();
	exit 1;;
