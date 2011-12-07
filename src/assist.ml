(* ASSIST.ML *)
(* It seems like a draft but there are a lot of function for all windows that
display exept the main window *)


(* -------------------------------------------------------------------------- *)
(* FENETRE DE FILTRES *)
(* -------------------------------------------------------------------------- *)

let exec_nop pict_view =
	Refe.filename := (Refe.get_orig_file ());
	pict_view#set_file (Refe.get_filename ())

let exec_so level pict_view =
  	let img = Sdlloader.load_image (Refe.get_filename ()) in
	begin
	if level = 1 then
		let img_so = (Filter.sobel_filter img) in
		Sdlvideo.save_BMP img_so "tmp.bmp";
	else (*level = 2 *)
		let img_so = (Filter.sobel_filter2 img) in
		Sdlvideo.save_BMP img_so "tmp.bmp";
	end;
	Refe.filename := "tmp.bmp";
	pict_view#set_file "tmp.bmp"

let view_img () =
	(*La fenetre de filtre *)
	if (Refe.get_filename ()) != "" then
	begin
	Refe.orig_file := (Refe.get_filename ());
	let win = GWindow.window
		~title:"Welcome" ()
		~width:800
		~height:570
		~position:`MOUSE in
	ignore (win#connect#destroy ~callback:(fun () -> ()));
	let hbox = GPack.hbox
		~packing:win#add () in
	(*les boutons de la fenetre de filtre*)
	let box = GPack.vbox
		~spacing:5
		~border_width:9
		~packing:hbox#add () in
	(*pour les encadrer*)
	let fram = GBin.frame
		~label:"Filters"
		~border_width:5
		~packing:box#pack () in
	(*pour mettre les boutons dans la frame*)
	let box_fram = GPack.vbox
		~spacing:5
		~border_width:5
		~packing:fram#add () in
	(*pas de filtre*)
	let btn_nop = GButton.button
		~label:"Disable filter"
		~packing:box_fram#add () in
	let box_so = GPack.hbox
		~spacing:5
		~packing:box_fram#add () in
	let btn_so1 = GButton.button
		~label:"Sobel 1"
		~packing:box_so#add () in
	let btn_so2 = GButton.button
		~label:"Sobel 2"
		~packing:box_so#add () in
	let _btn_3 = GButton.button
		~label:"unused"
		~packing:box_fram#add () in
	let _btn_4 = GButton.button
		~label:"unused"
		~packing:box_fram#add () in
	let _separator = GMisc.separator `HORIZONTAL
		~packing:box#add () in
	let btn = GButton.button
		~label:"Close"
		~packing:box#pack () in

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
	let picture = GMisc.image
		~file:(Refe.get_filename ())
		~packing:secbox#add () in


	(* -- CALLBACK -- *)
	ignore (btn_nop#connect#clicked
		~callback:(fun () -> (exec_nop picture)));
	ignore (btn_so1#connect#clicked
		~callback:(fun () -> (exec_so 1 picture)));
	ignore (btn_so2#connect#clicked
		~callback:(fun () -> (exec_so 2 picture)));
	ignore (btn#connect#clicked ~callback:(win#destroy));


	win#show ()
	end
	else
		()
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)




(* -------------------------------------------------------------------------- *)
(* LITTLE POP UP TO ENTER THE STEP *)
(* -------------------------------------------------------------------------- *)
let list_inutile_tbx = ref ([] : GEdit.entry list)

let fixstep () =
	match !list_inutile_tbx with
		| [] -> failwith "Error"
		| e::_ -> let t = e in
				try Refe.step := int_of_string (t#text);Pre.pre_trait () with
					| _ -> Refe.step := 5;
		Pre.pre_trait ()


(* WINDOW TO ENTER THE STEP *)
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

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)





(* -------------------------------------------------------------------------- *)
(* ALL FUNCTIONS FOR ENTER ALTITUDES *)
(* -------------------------------------------------------------------------- *)
type str_alt = Refe.struct_alt
type struct_tbx = {
	tbx : GEdit.entry;
	btn : GButton.color_button;
	orig_color : (int*int*int)
}
let stack = Stack.create ()

let rand_alt () =
	let alti = ref 0 in
	let i = ref 0 in
	let colors = ref (Refe.get_li ()) in
	while (!i < List.length (Refe.get_li ())) do
		let elt = List.hd !colors in
		let str = {
			Refe.alt = !alti;
			Refe.rgb = elt;
			Refe.orig_color = elt
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
	while not (Stack.is_empty stack) do
		let elt = Stack.pop stack in
		let gcolor = elt.btn#color in
		let r = (Gdk.Color.red gcolor) * 255 / 65535 and
		    g = (Gdk.Color.green gcolor) * 255 / 65535 and
			b = (Gdk.Color.blue gcolor) * 255 / 65535 in

		let str = {
			Refe.alt = (try int_of_string(elt.tbx#text) with
				| _ -> -1);
			Refe.rgb = (r, g, b);
			Refe.orig_color = elt.orig_color
		} in
		ignore (Refe.list_alt := str::(Refe.get_list_alt ()));
	done;
    Post.post_treat ();
    Graphics_engine.main_engine ()

(* WINDOW TO ENTER THE ALTS *)
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

	let sugar = ref 0 in
	for i=1 to List.length (Refe.get_li ()) do
		match (Refe.get_li ()) with
			| [] -> failwith "Critical error"
			| (r, g, b)::q -> let queue = q and
								  normal_r = r and
								  normal_g = g and
								  normal_b = b in
		Refe.li := queue;

		let btn_clr = GButton.color_button
			~color:(GDraw.color (`RGB ((normal_r*65535/255),
									   (normal_g*65535/255),
									   (normal_b*65535/255))))
			~packing:vbox1#add () in

		let tbx = GEdit.entry
			~text:(string_of_int !sugar)
			~max_length:4
			~width:4
			~packing:vbox2#add () in
		let str = {
			tbx = tbx;
			btn = btn_clr;
			orig_color = (normal_r, normal_g, normal_b)
		} in
		Stack.push str stack;
		sugar := !sugar + 5;
	done;
	let btn_ok = GButton.button
		~label:"OK"
		~packing:(vbox#pack ~padding:5) () in
	ignore (btn_ok#connect#clicked ~callback:(win1#destroy));
	win1#show ();
	(* end -- Generation des boutons en fonction de !nb_colors *)
