(* Calling original Perlin Noise. *)

external init_rand : int -> unit = "ext_init_random"
external c_perlin : float -> float -> float -> float -> int -> float = "ext_perlin"
external save_bmp :
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array3.t -> unit =
   "ext_save_bmp"
