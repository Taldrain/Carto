val exec_nop : < set_file : string -> 'a; .. > -> 'a
val exec_seq : < set_file : string -> 'a; .. > -> 'a
val exec_so : int -> < set_file : string -> 'a; .. > -> 'a
val exec_ult : 'a -> unit
val put_scale : < adjustment : < value : float; .. >; .. > -> unit
val destrof : < destroy : unit -> 'a; .. > -> < active : bool; .. > -> 'a
val view_img : unit -> unit
val rank : int ref
val stacky : Sdlvideo.surface Stack.t
val sdl_to_bmp : Sdlvideo.surface -> unit
val sdl_to_bmp_fin : Sdlvideo.surface -> unit
val exec_off :
  < misc : < set_sensitive : bool -> 'a; .. >; .. > ->
  < set_file : string -> 'b; .. > -> 'b
val exec_rerand : < set_file : string -> 'a; .. > -> 'a
val exec_wb : < set_file : string -> unit; .. > -> unit
val exec_aveg1 : < set_file : string -> unit; .. > -> unit
val exec_aveg2 : < set_file : string -> unit; .. > -> unit
val exec_gauss : < set_file : string -> unit; .. > -> unit
val exec_med1 : < set_file : string -> unit; .. > -> unit
val exec_med2 : < set_file : string -> unit; .. > -> unit
val exec_prec : < set_file : string -> unit; .. > -> unit
val exec_uflo : < set_file : string -> unit; .. > -> unit
val gridy : #GEdit.combo_box GEdit.text_combo -> unit
val destro :
  < destroy : unit -> 'a; .. > ->
  < active : bool; .. > -> < active : bool; .. > -> unit
val grise :
  < misc : < set_sensitive : bool -> 'a; .. >; .. > ->
  < misc : < set_sensitive : bool -> 'b; .. >; .. > ->
  < misc : < set_sensitive : bool -> 'c; .. >; .. > ->
  < misc : < set_sensitive : bool -> 'd; .. >; .. > ->
  < misc : < set_sensitive : bool -> 'e; .. >; .. > ->
  < misc : < set_sensitive : bool -> 'f; .. >; .. > ->
  < misc : < set_sensitive : bool -> 'g; .. >; .. > ->
  < misc : < set_sensitive : bool -> 'h; .. >; .. > -> 'h
val save : #GWindow.window_skel -> unit
val win_flout : unit -> unit
val list_inutile_tbx : GEdit.entry list ref
val fixstep : unit -> unit
val winstep : unit -> unit
type str_alt = Refe.struct_alt
type struct_tbx = {
  tbx : GEdit.entry;
  btn : GButton.color_button;
  orig_color : int * int * int;
}
val stack : struct_tbx Stack.t
val rand_alt : unit -> unit
val save_alt : unit -> unit
val winalt : unit -> unit
val aboutbox : unit -> unit
