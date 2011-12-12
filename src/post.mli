val li_ord : ((int * int * int) * int) list ref
val altitude : int * int * int -> int * (int * int * int)
val get_alt : unit -> unit
val red_pill : 'a array array -> int -> int -> bool
val get_h : ('a * 'b * 'c) * 'd -> 'c
val set_newH :
  'a -> ('b * 'c * 'd) * ('e * 'f * 'g) -> ('b * 'c * 'a) * ('e * 'f * 'g)
val liss : unit -> unit
val get_f : int -> int -> (int * int * int) * (int * int * int)
val mat_to_lixyz : unit -> ((int * int * int) * (int * int * int)) list
val mat_to_li : unit -> ((int * int * int) * (int * int * int)) list
val i_2_f :
  ((int * int * int) * (int * int * int)) list ->
  ((float * float * float) * (float * float * float)) list
val list_tolist2 : ('a * 'b) list -> ('a * int) list
val ident_elt : 'a -> ('a * int) list -> int
val ident_li :
  ((float * float * float) * 'a) list ->
  ((int * int * int) * int) list -> int list
val str_of_tri : int * int * int -> string
val write_obj : unit -> unit
val x0 : int ref
val y0 : int ref
val z0 : int ref
val xx : int ref
val yx : int ref
val xy : int ref
val yy : int ref
val list_in_tri_coords : (int * int) list ref
val stock_in_tri_coordsX : int -> int -> unit
val stock_in_tri_coordsY : int -> int -> unit
val test_membership_3 : 'a -> 'b -> ('a * 'b) list -> bool
val test_x : int -> int -> bool
val test_y : int -> int -> bool
val stock_tri3D : int -> int -> int -> int -> int -> int -> unit
val new_triangles : int -> int -> unit
val triangulation : unit -> unit
val print_liste :
  ((float * float * float) * (float * float * float)) list -> unit
val post_treat : unit -> unit
