(**************************************************************************)
(*    Carto - TopoTeam                                                    *)
(**************************************************************************)

(* BEGIN -- Main and various functions for the GTK interface *)
let quit () =
	GMain.quit ();
	exit 0


let exec_fst_treat btn =
	Assist.winstep ();
	btn#misc#set_sensitive true

let exec_assist () =
	Assist.winalt ()

let exec_brow win b_img b_obj =
	Browser.browser win;
	if (Refe.get_if_file ()) then
	begin
        Refe.orig_file := (Refe.get_filename ());
		if ((Refe.get_file_type ()) = "obj") then
			b_obj#misc#set_sensitive true
		else
            begin
  	        Assist.win_flout ();
			b_img#misc#set_sensitive true
            end
	end

let exec_random btn =
    begin
	if ((Sys.command "./genperlin -save > rand_map.bmp") = 0) then
		(Refe.filename := "rand_map.bmp";
        Refe.rand_file := true;)
	else
		failwith "Fatal error on genperlin"
	end;
  	Assist.win_flout ();
	btn#misc#set_sensitive true



let exec_3d_obj () =
	(* PETAGE DU MOTEUR 3D *)
	Parser_obj.open_obj ();
	Parser_obj.put_color ();
	Graphics_engine.main_engine ()

let exec_3d_inst () =
	(* 3D POUR LES PRESSES *)
	begin
	if ((Sys.command "./genperlin -save > rand_map.bmp") = 0) then
		Refe.filename := "rand_map.bmp"
	else
		Refe.filename := "carte.bmp"
	end;
	(*print_endline (Refe.get_filename ());*)
  	Assist.view_img ();
	Refe.file_type := "img";
	Refe.step := 5;
	Pre.pre_trait ();
	Assist.rand_alt ()

let main () =

	ignore (GtkMain.Main.init ());

	(* Recuperation de la taille de l'ecran *)
  	let w = GWindow.window
		~title:"Carto TopoTeam" ()
		(*~width:200
		~height:400*)
		~position:`CENTER in
	let main_box = GPack.vbox
		~packing:w#add () in

	(* Menu *)
	let menufile = GMenu.menu () in
	let mf_open = GMenu.menu_item
		~label:"Open image"
		~packing:menufile#append () in
	let mf_quit = GMenu.menu_item
		~label:"Quit"
		~packing:menufile#append () in
	let menubar = GMenu.menu_bar
		~packing:main_box#add () in
	let item1 = GMenu.menu_item
		~label:"File"
		~packing:menubar#append () in
	let menuhelp = GMenu.menu () in
    let mf_about = GMenu.menu_item
        ~label:"About"
		~packing:menuhelp#append () in
	let item2 = GMenu.menu_item
		~label:"Help"
		~packing:menubar#append () in
	item1#set_submenu menufile;
    item2#set_submenu menuhelp;

	let _lbl = GMisc.label
		~text:"Projet Carto -- Topo team"
		~packing:main_box#pack () in
	let _lbl = GMisc.label
		~text:"\nEffectuer le pre traitement\navant de lancer l'assistant\n"
		~packing:main_box#pack () in
	let box_open = GPack.hbox
		~spacing:5
		~packing:main_box#pack () in
	let btn_browse = GButton.button
		~label:"Browse"
		~packing:box_open#add () in
	let btn_rand = GButton.button
		~label:"Random"
		~packing:box_open#add () in
    let _sep = GMisc.separator `HORIZONTAL
		~packing:main_box#pack () in
    let _sep = GMisc.separator `HORIZONTAL
		~packing:main_box#pack () in
    let _sep = GMisc.separator `HORIZONTAL
		~packing:main_box#pack () in
	let btn_pre_treat = GButton.button
		~label:"Execute"
		~packing:main_box#pack () in
	let btn_assist = GButton.button
		~label:"Assist"
		~packing:main_box#pack () in
	let btn_3d_obj = GButton.button
		~label:"3D OBJ"
		~packing:main_box#pack () in
    let _sep = GMisc.separator `HORIZONTAL
		~packing:main_box#pack () in
    let _sep = GMisc.separator `HORIZONTAL
		~packing:main_box#pack () in
    let _sep = GMisc.separator `HORIZONTAL
		~packing:main_box#pack () in
	let btn_3d_inst = GButton.button
		~label:"3D instantane"
		~packing:main_box#pack () in
	let btn_quit = GButton.button
		~label:"Quit"
	 	~packing:main_box#pack () in


	btn_pre_treat#misc#set_sensitive false;
	btn_assist#misc#set_sensitive false;
	btn_3d_obj#misc#set_sensitive false;

	(* --------- *)
	(* CALLBACKS *)
	(* --------- *)

	(*menu*)
	ignore (mf_open#connect#activate
		~callback:(fun () -> exec_brow w btn_pre_treat btn_3d_obj));
	ignore (mf_quit#connect#activate
		~callback:quit);
	ignore (mf_about#connect#activate
		~callback:(Assist.aboutbox));

	(*buttons*)
  	ignore (w#connect#destroy ~callback:GMain.quit);
	ignore (btn_browse#connect#clicked
		~callback:(fun () -> exec_brow w btn_pre_treat btn_3d_obj));
	ignore (btn_rand#connect#clicked
		~callback:(fun () -> exec_random btn_pre_treat));

	ignore (btn_pre_treat#connect#clicked
		~callback:(fun () -> exec_fst_treat btn_assist));
	ignore (btn_assist#connect#clicked
		~callback:exec_assist);
	ignore (btn_3d_obj#connect#clicked
		~callback:exec_3d_obj);
	ignore (btn_3d_inst#connect#clicked
		~callback:exec_3d_inst);
	ignore (btn_quit#connect#clicked
		~callback:quit);

  	w#show ();
  	GMain.main ()

