val default : 'a -> 'a option -> 'a
val all_files : unit -> GFile.filter
val is_string_prefix : string -> string -> bool
val image_filter : unit -> GFile.filter
val obj_filter : unit -> GFile.filter
val browser : #GWindow.window_skel -> unit
