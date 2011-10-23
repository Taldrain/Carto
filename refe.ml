let filename = ref ("")
let get_filename () = !filename

let nb_colors = ref 7
let get_nb_colors () = !nb_colors

let li = ref ( [] : (int*int*int) list )
let get_li () = !li

type struct_alt =
{
  alt : int;
  rgb : (int*int*int)
}

let list_alt = ref ([] : struct_alt list)
let get_list_alt () = !list_alt

let step = ref 5
let get_step() = !step

let pos = ref 0
let get_pos() = !pos

let w = ref 600
let get_w() = !w

let h = ref 600
let get_h() = !h

let matrice = ref (Array.make_matrix
		     ((!w)/(!step) + 1)
		     ((!h)/(!step) + 1) (0,0))
let get_matrice() = !matrice

let matrice_rgb = ref (Array.make_matrix
			 ((!w)/(!step) + 1)
			 ((!h)/(!step) + 1)
			 ((0,0),(0,0,0)))
let get_matrice_rgb() = !matrice_rgb

let matrice_ret = ref (Array.make_matrix
		     ((!w)/(!step) + 1)
		     ((!h)/(!step) + 1) ((0,0,0),(0,0,0)))
let get_matrice_ret() = !matrice_ret


let list_3d = ref [((0.,0.,0.),(0.,0.,0.))]
let get_list_3d() = !list_3d
