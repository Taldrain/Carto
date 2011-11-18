(* Parser le .obj - deuxième version *)

let l_topCoords = ref ([]:(float*float*float) list)
let l_textureCoords = ref ([]:(float*float) list)
let l_normCoords = ref ([]:(float*float*float) list)
let l_face = ref ([]:(string*string*string) list)
module Str = String


let stock_list_triplet l1 l2 =
	let a = float_of_string (List.nth l1 1) and
		b = float_of_string (List.nth l1 2) and
		c = float_of_string (List.nth l1 3) in
			l2 := (a,b,c)::(!l2)


let stock_list_couple l1 l2 =
	let a = float_of_string (List.nth l1 1) and
		b = float_of_string (List.nth l1 2) in
			(a,b)::l2


let stock_list_face l1 l2 =
	let a = List.nth l1 1 and
		b = List.nth l1 2 and
		c = List.nth l1 3 in
			(a,b,c)::l2


let open_obj () =
let obj_file = open_in "supermap.obj" in
	let rec parser_obj ()  =
	let line = input_line obj_file in
		let string_list = Str.split (Str.regexp " ") line in
		begin
			match List.hd string_list with
			| "v" ->  stock_list_triplet string_list l_topCoords
		(*	| "vt" -> stock_list_couple string_list !l_textureCoords*)
		(*	| "vn" -> stock_list_triplet string_list !l_normCoords*)
		(*	| "f" -> stock_list_face string_list !l_face*)
			| _ -> ()
		end;
		parser_obj () in
	parser_obj ()
