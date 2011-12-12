val l_topCoords : (float * float * float) list ref
val l_textureCoords : (float * float) list ref
val l_normCoords : (float * float * float) list ref
val l_face : (string * string * string) list ref
val stock_list_triplet :
  string list -> (float * float * float) list ref -> unit
val stock_list_couple : string list -> (float * float) list ref -> unit
val stock_list_face : 'a list -> ('a * 'a * 'a) list ref -> unit
val open_obj : unit -> unit
val affich : ((float * float * float) * (float * float * float)) list -> unit
val put_colors : 'a list -> ('a * (float * float * float)) list
val put_color : unit -> unit
