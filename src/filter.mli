val img_to_mat : Sdlvideo.surface -> Sdlvideo.color array array
val sobel1 : unit -> int array array
val sobel2 : unit -> int array array
val sobelv2_1 : unit -> int array array
val sobelv2_2 : unit -> int array array
val gauss3 : unit -> int array array
val gauss5 : unit -> int array array
val average3 : unit -> int array array
val average5 : unit -> int array array
val booster : unit -> int array array
val atb_p : Sdlvideo.surface -> int -> int -> string -> int
val normalize : int * int * int -> int * int * int
val mpf3 :
  Sdlvideo.surface -> int -> int -> int array array -> string -> int -> int
val multi3_pix_filter :
  Sdlvideo.surface -> int -> int -> int array array -> int -> int * int * int
val mpf5 :
  Sdlvideo.surface -> int -> int -> int array array -> string -> int -> int
val multi5_pix_filter :
  Sdlvideo.surface -> int -> int -> int array array -> int -> int * int * int
val multi_mat_filter :
  int array array ->
  Sdlvideo.surface -> int -> int -> (int * int * int) array array
val mat_to_img :
  Sdlvideo.color array array -> Sdlvideo.surface -> Sdlvideo.surface
val mat1_mat2 :
  (int * int * int) array array ->
  (int * int * int) array array -> 'a -> (int * int * int) array array
val filtr_simpl_img :
  Sdlvideo.surface -> int array array -> int -> int -> Sdlvideo.surface
val filtr_doubl_img :
  Sdlvideo.surface ->
  int array array ->
  int array array -> int -> int -> (int * int * int) array array
val mat_edge_to_white :
  Sdlvideo.color array array -> Sdlvideo.surface -> Sdlvideo.surface
val img_to_grey : Sdlvideo.surface -> Sdlvideo.surface
val sobel_filter : Sdlvideo.surface -> Sdlvideo.surface
val sobel_filter2 : Sdlvideo.surface -> Sdlvideo.surface
val red : ('a * 'b * 'c) array array -> int -> int -> 'a
val qsort : ('a * 'b * 'c) list -> 'a -> 'b * 'c
val median_filtr : Sdlvideo.surface -> int -> Sdlvideo.surface
val sobel_filter_f_color : Sdlvideo.surface -> Sdlvideo.surface
val sobel_filter_f : Sdlvideo.surface -> Sdlvideo.surface
val average1 : Sdlvideo.surface -> Sdlvideo.surface
val average2 : Sdlvideo.surface -> Sdlvideo.surface
val gauss3_filter : Sdlvideo.surface -> Sdlvideo.surface
val median_filtr5 : Sdlvideo.surface -> Sdlvideo.surface
val median_filtr3 : Sdlvideo.surface -> Sdlvideo.surface
val boost_filtr : Sdlvideo.surface -> Sdlvideo.surface
