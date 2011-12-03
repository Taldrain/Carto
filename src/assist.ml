(***************************************************************************)
(*		L'assistant pour les couleurs dans un premier temps	   *)
(***************************************************************************)
type str_alt = Refe.struct_alt
type struct_tbx = {
	tbx : GEdit.entry;
	color : (int*int*int)
}
let list_tbx = ref ([] : struct_tbx list)
let list_inutile_tbx = ref ([] : GEdit.entry list)

let rand_alt () =
	let alti = ref 0 in
	let i = ref 0 in
	let colors = ref (Refe.get_li ()) in
	while (!i < List.length (Refe.get_li ())) do
		let elt = List.hd !colors in
		let str = {
			Refe.alt = !alti;
			Refe.rgb = elt
		} in
		alti := !alti + 5;
		colors := List.tl (!colors);
		i := !i + 1;
		ignore (Refe.list_alt := str::(Refe.get_list_alt ()));
	done;
    Post.post_treat ();
    Graphics_engine.main_engine ()

(* Formulaire de demande d'altitude *)
let save_alt () =
	(*print_endline (string_of_int (List.length !list_tbx));*)
	while List.length !list_tbx != 0 do
		let elt = List.hd !list_tbx in
		let str = {
			Refe.alt = (try int_of_string(elt.tbx#text) with
				| _ -> -1);
			Refe.rgb = elt.color
		} in
		list_tbx := List.tl !list_tbx;
		ignore (Refe.list_alt := str::(Refe.get_list_alt ()));
	done;
    Post.post_treat ();
    Graphics_engine.main_engine ()
	
let view_img () =
	let win = GWindow.window
		~title:"Welcome" ()
		~width:800
		~height:570
		~position:`MOUSE in
	ignore (win#connect#destroy ~callback:(fun () -> ()));
	let hbox = GPack.hbox
		~packing:win#add () in

	let box = GPack.vbox
		~packing:hbox#add () in
	let _btn_1 = GButton.button
		~label:"1"
		~packing:box#add () in
	let _btn_2 = GButton.button
		~label:"2"
		~packing:box#add () in
	let _btn_3 = GButton.button
		~label:"3"
		~packing:box#add () in

	let box2 = GPack.vbox
		~packing:hbox#add () in
	let scrolled_window = GBin.scrolled_window
		~border_width:10
		~hpolicy:`AUTOMATIC
		~vpolicy:`AUTOMATIC
		~width:512
		~height:512
		~packing:box2#add () in
	let secbox = GPack.hbox
		~packing:scrolled_window#add_with_viewport () in
	let _img = GMisc.image
		~file:(Refe.get_filename ())
		~packing:secbox#add () in
	let btn = GButton.button
		~label:"Close"
		~packing:box#add () in
	ignore (btn#connect#clicked ~callback:(win#destroy));
	win#show ()
	



let fixstep () =
	match !list_inutile_tbx with
		| [] -> failwith "Error"
		| e::_ -> let t = e in
				try Refe.step := int_of_string (t#text);Pre.pre_trait () with
					| _ -> Refe.step := 5;
		Pre.pre_trait ()


(* old name: firstwin *)
let winstep () =
	let win = GWindow.window
		~title:"Welcome" ()
		~width:300
		~height:100
		~position:`CENTER in
	ignore (win#connect#destroy ~callback:(fixstep));
	let box = GPack.vbox
		~packing:win#add () in
	let nd_box = GPack.hbox
		~packing:box#add () in
	let _lbl = GMisc.label
		~text:"Entry the step"
		~packing:nd_box#add () in
	(* Textbox *)
	let tbx = GEdit.entry
			~max_length:4
			~width:4
			~packing:nd_box#add () in
	ignore (list_inutile_tbx := tbx::!list_inutile_tbx);
	let btn_ok = GButton.button
		~label:"OK"
		~packing:box#add () in
	ignore (btn_ok#connect#clicked ~callback:(win#destroy));
	win#show ()

(* old name : first*)
let winalt () =
	let win1 = GWindow.window
		~title:"Assist first step" ()
		~width:800
		~height:600
		~position:`CENTER in
	ignore (win1#connect#destroy ~callback:(save_alt));
	let vbox = GPack.vbox
		~packing:win1#add () in
	let _lbl = GMisc.label
		~text:"Enter the colors for each level"
		~packing:vbox#add () in
	let _separator = GMisc.separator `HORIZONTAL
		~packing:vbox#add () in
	(* --------------------------------------- *)
	let scrolled_window = GBin.scrolled_window
		~border_width:10
		~hpolicy:`AUTOMATIC
		~vpolicy:`AUTOMATIC
		~packing:vbox#add () in
	let secbox = GPack.hbox
		~packing:scrolled_window#add_with_viewport () in
	let vbox1 = GPack.vbox
		~packing:secbox#add () in
	let vbox2 = GPack.vbox
		~packing:secbox#add () in

	(* begin -- Generation des boutons en fonction de !nb_colors *)
	Pre.sdl_init ();
	let img_ref = Sdlloader.load_image "img/ref.png" in
	let (w, h) = ((Sdlvideo.surface_info img_ref).Sdlvideo.w,
				((Sdlvideo.surface_info img_ref).Sdlvideo.h)) in
	let sugar = ref 0 in
	for i=1 to List.length (Refe.get_li ()) do
	ignore (
		let carname = ("img/car"^(string_of_int i)^".bmp") in
		let cartouche =
			Sdlvideo.create_RGB_surface_format img_ref [] w h in
		match (Refe.get_li ()) with
		| [] -> failwith "Critical error"
		| e::q -> let rgb = e and queue = q in
		Refe.li := queue;
		for y=0 to (h-1) do
		for x=0 to (w-1) do
			Sdlvideo.put_pixel_color cartouche x y rgb;
		done;
		done;
		Sdlvideo.save_BMP cartouche carname;
		let _img = GMisc.image
			~file:carname
			~packing:vbox1#add () in
		let tbx = GEdit.entry
			~text:(string_of_int !sugar)
			~max_length:4
			~width:4
			~packing:vbox2#add () in
		let str = {
			tbx = tbx;
			color = e
		} in
		list_tbx := str::(!list_tbx);
		sugar := !sugar + 5;
	)
	done;
	let btn_ok = GButton.button
		~label:"OK"
		~packing:(vbox#pack ~padding:5) () in
	ignore (btn_ok#connect#clicked ~callback:(win1#destroy));
	win1#show ();
	(* end -- Generation des boutons en fonction de !nb_colors *)

