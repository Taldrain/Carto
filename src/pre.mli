val get_dims : Sdlvideo.surface -> unit
val sdl_init : unit -> unit
val wait_key : unit -> unit
val show : Sdlvideo.surface -> Sdlvideo.surface -> unit
val p_list : 'a list -> 'a -> 'a list
val str_of_tri : int * int * int -> string
val stringList : (int * int * int) list -> string list
val list_to_text : (int * int * int) list -> unit
val tolc : int -> int -> bool
val tolerance : int * int * int -> int * int * int -> bool
val contour : Sdlvideo.surface -> Sdlvideo.surface
val contour_hor : Sdlvideo.surface -> int -> unit
val contour_ver : Sdlvideo.surface -> int -> unit
val contour_diag1 : Sdlvideo.surface -> int -> unit
val map_to_mat : int -> int -> int -> int -> unit
val matXY_to_matRGB : Sdlvideo.surface -> unit
val pre_trait : unit -> unit
