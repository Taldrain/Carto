val quit : unit -> 'a
val reset : unit -> unit
val exec_fst_treat : < misc : < set_sensitive : bool -> 'a; .. >; .. > -> 'a
val exec_assist : < misc : < set_sensitive : bool -> 'a; .. >; .. > -> 'a
val exec_brow :
  #GWindow.window_skel ->
  < misc : < set_sensitive : bool -> unit; .. >; .. > ->
  < misc : < set_sensitive : bool -> unit; .. >; .. > -> unit
val exec_random :
  < misc : < set_sensitive : bool -> 'a; .. >; .. > ->
  < misc : < set_sensitive : bool -> 'b; .. >; .. > -> 'b
val exec_glgtk : unit -> unit
val exec_3d_obj : unit -> unit
val exec_3d_inst : unit -> unit
val main : unit -> unit
