let filename = ref ("")
let get_filename () = !filename

let orig_file = ref ("")
let get_orig_file () = !orig_file

let file_type = ref ("")
let get_file_type () = !file_type

let if_file = ref false
let get_if_file () = !if_file

let rand_file = ref false
let get_rand_file () = !rand_file

let nb_colors = ref 7
let get_nb_colors () = !nb_colors

let li = ref ( [] : (int*int*int) list )
let get_li () = !li

type struct_alt =
{
  alt : int;
  rgb : (int*int*int);
  orig_color : (int*int*int)
}

let list_alt = ref ([] : struct_alt list)
let get_list_alt () = !list_alt

let step = ref 5
let get_step() = !step

let save_color_txt = ref false
let save_ornot_color () = !save_color_txt

let save_obj = ref false
let is_save_obj () = !save_obj

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

let matrice_fin = ref (Array.make_matrix
		     ((!w)/(!step) + 1)
		     ((!h)/(!step) + 1) ((0,0,0),(0,0,0)))
let get_matrice_fin() = !matrice_fin

let list_3d = ref [((0.,0.,0.),(0.,0.,0.))]
let get_list_3d() = !list_3d

let list_xyz = ref [((0,0,0),(0,0,0))]
let get_list_xyz() = !list_xyz


let list_tri3D = ref ( [] : ((float*float*float)*(float*float*float)) list )
let get_list_tri3D () = !list_tri3D

let version = ref "Supermap, version RC-1"
let gVersion = !version

let grid_stat = ref "Continue"
let get_grid_stat() = !grid_stat

let tolerance = ref 5
let get_tolerance() = !tolerance

let perso = ref false
let get_perso () = !perso

let wg = ref false
let g_wg () = !wg
