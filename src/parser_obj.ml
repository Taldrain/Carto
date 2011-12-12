(* Parser le .obj - deuxième version *)

let l_topCoords = ref ([]:(float*float*float) list)
let l_textureCoords = ref ([]:(float*float) list)
let l_normCoords = ref ([]:(float*float*float) list)
let l_face = ref ([]:(string*string*string) list)


let stock_list_triplet l1 l2 =
	let a = float_of_string (List.nth l1 1) in
	let	b = float_of_string (List.nth l1 2) in
	let	c = float_of_string
		(String.sub (List.nth l1 3) 0 ((String.length (List.nth l1 3)) - 2)) in
			l2 := (a,b,c)::(!l2)


let stock_list_couple l1 l2 =
	let a = float_of_string (List.nth l1 1) in
	let	b = float_of_string
		 (String.sub (List.nth l1 2) 0 ((String.length (List.nth l1 2)) - 2)) in
			l2 := (a,b)::(!l2)


let stock_list_face l1 l2 =
	let a = List.nth l1 1 in
	let	b = List.nth l1 2 in
	let	c = List.nth l1 3 in
			l2 := (a,b,c)::(!l2)


let open_obj () =
let obj_file = open_in (Refe.get_filename ()) in
	let rec parser_obj ()  =
	let line = input_line obj_file in
		let string_list = Str.split (Str.regexp " ") line in
		begin
			match List.hd string_list with
			| "v" -> stock_list_triplet string_list l_topCoords
			| "vt" -> stock_list_couple string_list l_textureCoords
			| "vn" -> stock_list_triplet string_list l_normCoords
			| "f" -> stock_list_face string_list l_face
			| _ -> ()
		end;
		parser_obj () in

	try
	parser_obj ()
	with _ -> print_endline "Little exception not important"

let rec affich = function
	| [] -> ()
	| ((a, b, c), (d, e, f))::l ->
		(Printf.printf "%s" (string_of_float a));
		Printf.printf ",";
		(Printf.printf "%s" (string_of_float b));
		Printf.printf ",";
		(Printf.printf "%s" (string_of_float c));
		Printf.printf ",";
		(Printf.printf "%s" (string_of_float d));
		Printf.printf ",";
		(Printf.printf "%s" (string_of_float e));
		Printf.printf ",";
		(Printf.printf "%s" (string_of_float f));
		Printf.printf ",";
		affich l


let rec put_colors = function
	| [] -> []
	| e::l -> (e, (255., 0., 0.))::(put_colors l)

let put_color () =
	Refe.list_3d := (put_colors !l_topCoords)
	(*affich (Refe.get_list_3d ());*)
	(*print_endline ""*)
