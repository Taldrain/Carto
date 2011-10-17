(***************************************************************************)
(*		L'assistant pour les couleurs dans un premier temps 			   *)
(***************************************************************************)

type struct_alt = {
	lbl : GMisc.label;
	img : GMisc.image;
	tbx : GEdit.entry
}
let list_lbltbx_struct = ref ([] : struct_alt list)

(* Formulaire de demande d'altitude *)


let first () =
	let win1 = GWindow.window 
		~title:"Assist first step" () 
		~width:800 
		~height:600 in
	win1#connect#destroy ~callback:GMain.quit;
	let vbox = GPack.vbox 
		~packing:win1#add () in
	let lbl = GMisc.label 
		~text:"Enter the colors for each level"
		~packing:vbox#add () in
	let separator = GMisc.separator `HORIZONTAL 
		~packing:vbox#add () in
	(* --------------------------------------- *)
	let scrolled_window = GBin.scrolled_window
		~border_width:10
		~hpolicy:`AUTOMATIC
		~vpolicy:`AUTOMATIC
		~packing:vbox#add () in
	let secvbox = GPack.vbox 
		~packing:scrolled_window#add_with_viewport () in
	let hbox1 = GPack.hbox
		~packing:secvbox#add () in
	let hbox2 = GPack.hbox
		~packing:secvbox#add () in
	let hbox3 = GPack.hbox
		~packing:secvbox#add () in

	(* begin -- Generation des boutons en fonction de !nb_colors *)
	Pre.sdl_init ();
	let img_ref = Sdlloader.load_image "img/ref.png" in
	let (w, h) = ((Sdlvideo.surface_info img_ref).Sdlvideo.w, 
				((Sdlvideo.surface_info img_ref).Sdlvideo.h)) in
	for i=1 to (Refe.get_nb_colors ()) do
		let carname = ("img/car"^(string_of_int i)^".bmp") in
		let cartouche = 
			Sdlvideo.create_RGB_surface_format img_ref [] w h in
		let rgb = List.hd (Refe.get_li ()) in
		Refe.li := List.tl (Refe.get_li ()); 
		for y=0 to h do
		for x=0 to w do
			Sdlvideo.put_pixel_color cartouche x y rgb;
			Sdlvideo.save_BMP cartouche carname
		done
		done;
		let temp = {
			lbl = GMisc.label
				~text:("Color "^(string_of_int i))
				~packing:hbox1#add ();
			img = GMisc.image
				~file:carname
				~packing:hbox2#add ();
			tbx = GEdit.entry
				~max_length:4
				~width:4
				~packing:hbox3#add ()
		} in
			temp::(!list_lbltbx_struct)
	done;
	win1#show ()
	(* end -- Generation des boutons en fonction de !nb_colors *)

