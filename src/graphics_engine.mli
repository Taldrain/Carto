val xold : float ref
val yold : float ref
val bl_down : bool ref
val zold : float ref
val rzold : float ref
val br_down : bool ref
val pas : float ref
type mode = Fill | Line | Point
val mode_ : mode ref
val g_mode : mode -> [> `fill | `line | `point ]
val fullscreen : bool ref
val rx : float ref
val ry : float ref
val rz : float ref
val dtx : unit -> float
val tx : float ref
val dty : unit -> float
val ty : float ref
val dtz : unit -> float
val tz : float ref
val lx : float ref
val ly : float ref
val lz : float ref
val monte : bool ref
val light_b : bool ref
val init_b : bool ref
val set_init : unit -> unit
val get_init : unit -> bool
val reset : unit -> unit
val init : unit -> unit
val init_light : unit -> unit
val dn : unit -> unit
val gColor : float * float * float -> unit
val create_tri : (Gl.point3 * (float * float * float)) list -> unit
val display : unit -> unit
val xor : bool -> bool -> bool
val motion : float -> float -> unit
val act_leftDown : float -> float -> unit
val act_leftUp : unit -> unit
val act_rightDown : float -> float -> unit
val act_rightUP : unit -> unit
val act_i : unit -> unit
val act_k : unit -> unit
val act_j : unit -> unit
val act_l : unit -> unit
val act_u : unit -> unit
val act_o : unit -> unit
val act_w : unit -> unit
val act_f : unit -> unit
val act_p : unit -> unit
val act_q : unit -> unit
val act_e : unit -> unit
val act_r : unit -> unit
val act_g : unit -> unit
val act_2 : unit -> unit
val act_3 : unit -> unit
val act_4 : unit -> unit
val act_6 : unit -> unit
val act_8 : unit -> unit
val act_9 : unit -> unit
val act_keyLeft : unit -> unit
val act_keyRight : unit -> unit
val act_keyDown : unit -> unit
val act_keyUP : unit -> unit
