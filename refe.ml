let filename = ref ("")
let get_filename () = !filename

let nb_colors = ref 7
let get_nb_colors () = !nb_colors

let li = ref ( [] : (int*int*int) list )
let get_li () = !li

type struct_alt = {
	alt : int;
    rgb : (int*int*int)
}
let list_alt = ref ([] : struct_alt list)
let get_list_alt () = !list_alt
