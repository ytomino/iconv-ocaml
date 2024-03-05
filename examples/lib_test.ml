let verbose = Array.length Sys.argv > 1 && Sys.argv.(1) = "--verbose";;

let f file line fmt v = (
	if verbose then Printf.printf ("%s:%d: " ^^ fmt ^^ "\n%!") file line v;
	v
);;
