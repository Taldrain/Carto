val filename : string ref
val get_filename : unit -> string
val orig_file : string ref
val get_orig_file : unit -> string
val file_type : string ref
val get_file_type : unit -> string
val if_file : bool ref
val get_if_file : unit -> bool
val rand_file : bool ref
val get_rand_file : unit -> bool
val nb_colors : int ref
val get_nb_colors : unit -> int
val li : (int * int * int) list ref
val get_li : unit -> (int * int * int) list
type struct_alt = {
  alt : int;
  rgb : int * int * int;
  orig_color : int * int * int;
}
val list_alt : struct_alt list ref
val get_list_alt : unit -> struct_alt list
val step : int ref
val get_step : unit -> int
val save_color_txt : bool ref
val save_ornot_color : unit -> bool
val save_obj : bool ref
val is_save_obj : unit -> bool
val w : int ref
val get_w : unit -> int
val h : int ref
val get_h : unit -> int
val matrice : (int * int) array array ref
val get_matrice : unit -> (int * int) array array
val matrice_rgb : ((int * int) * (int * int * int)) array array ref
val get_matrice_rgb : unit -> ((int * int) * (int * int * int)) array array
val matrice_ret : ((int * int * int) * (int * int * int)) array array ref
val get_matrice_ret :
  unit -> ((int * int * int) * (int * int * int)) array array
val matrice_fin : ((int * int * int) * (int * int * int)) array array ref
val get_matrice_fin :
  unit -> ((int * int * int) * (int * int * int)) array array
val list_3d : ((float * float * float) * (float * float * float)) list ref
val get_list_3d :
  unit -> ((float * float * float) * (float * float * float)) list
val list_xyz : ((int * int * int) * (int * int * int)) list ref
val get_list_xyz : unit -> ((int * int * int) * (int * int * int)) list
val list_tri3D : ((float * float * float) * (float * float * float)) list ref
val get_list_tri3D :
  unit -> ((float * float * float) * (float * float * float)) list
val version : string ref
val gVersion : string
val grid_stat : string ref
val get_grid_stat : unit -> string
val tolerance : int ref
val get_tolerance : unit -> int
val perso : bool ref
val get_perso : unit -> bool
val wg : bool ref
val g_wg : unit -> bool
