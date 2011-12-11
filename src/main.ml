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
	if ((Sys.command "./genperlin -save > /tmp/rand_map.bmp") = 0) then
		(Refe.filename := "/tmp/rand_map.bmp";
        Refe.orig_file := (Refe.get_filename ());
        Refe.rand_file := true;)
	else
		failwith "Fatal error on genperlin"
	end;
  	Assist.win_flout ();
	btn#misc#set_sensitive true


let exec_glgtk () =
  	let w = GWindow.window
		~title:"Carto TopoTeam" ()
		~width:1024
		~height:800
		~position:`CENTER
        ~show:true in
    let box = GPack.vbox
        ~packing:w#add () in
    let area = GlGtk.area [`RGBA;`DOUBLEBUFFER;`DEPTH_SIZE 1;`USE_GL]
        ~width:1024
        ~height:768
        ~packing:box#add () in
    let btn = GButton.button
        ~label:"Close"
        ~packing:box#add () in
    ignore (area#connect#realize ~callback:Graphics_engine.init);
    ignore (area#connect#display ~callback:(fun () -> (Graphics_engine.display ();
                                               area#swap_buffers ())));
    area#event#add [`ALL_EVENTS];


    ignore(
   area#event#connect#button_press ~callback:
    begin fun ev ->
      let button = GdkEvent.Button.button ev in
        if button = 1 then Graphics_engine.act_leftDown (GdkEvent.Button.x ev)
                           (GdkEvent.Button.y ev) else
        if button = 3 then Graphics_engine.act_rightDown (GdkEvent.Button.x ev)
                           (GdkEvent.Button.y ev) ;
        Graphics_engine.display ();
        area#swap_buffers ();
          true
    end);

    ignore(
   area#event#connect#motion_notify ~callback:
    begin fun ev ->
      let motion = GdkEvent.Motion.is_hint ev in
        if motion then Graphics_engine.motion (GdkEvent.Motion.x ev)
                               (GdkEvent.Motion.y ev) ;
        Graphics_engine.display ();
        area#swap_buffers ();
          true
    end);

    ignore(
   area#event#connect#button_release ~callback:
    begin fun ev ->
      let button = GdkEvent.Button.button ev in
        if button = 1 then Graphics_engine.act_leftUp () else
        if button = 3 then Graphics_engine.act_rightUP () ;
        Graphics_engine.display ();
        ignore (area#swap_buffers ());
          true
    end);

    ignore(
   w#event#connect#key_press ~callback:
    begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
        if key = GdkKeysyms._Escape then (Refe.list_tri3D := [];
										  Refe.list_3d := [];
										  area#destroy (); w#destroy ();
                                          Graphics_engine.set_init ()) else
        if key = GdkKeysyms._Down then Graphics_engine.act_keyDown () else
        if key = GdkKeysyms._Up then Graphics_engine.act_keyUP () else
        if key = GdkKeysyms._Right then Graphics_engine.act_keyRight () else
        if key = GdkKeysyms._Left then Graphics_engine.act_keyLeft () else
        if key = GdkKeysyms._i then Graphics_engine.act_i () else
        if key = GdkKeysyms._k then Graphics_engine.act_k () else
        if key = GdkKeysyms._j then Graphics_engine.act_j () else
        if key = GdkKeysyms._l then Graphics_engine.act_l () else
        if key = GdkKeysyms._u then Graphics_engine.act_u () else
        if key = GdkKeysyms._o then Graphics_engine.act_o () else
        if key = GdkKeysyms._w then Graphics_engine.act_w () else
        if key = GdkKeysyms._f then Graphics_engine.act_f () else
        if key = GdkKeysyms._p then Graphics_engine.act_p () else
        if key = GdkKeysyms._q then Graphics_engine.act_q () else
        if key = GdkKeysyms._e then Graphics_engine.act_e () else
        if key = GdkKeysyms._r then Graphics_engine.act_r () else
        if key = GdkKeysyms._g then Graphics_engine.act_g () else
        if key = GdkKeysyms._2 then Graphics_engine.act_2 () else
        if key = GdkKeysyms._3 then Graphics_engine.act_3 () else
        if key = GdkKeysyms._4 then Graphics_engine.act_4 () else
        if key = GdkKeysyms._6 then Graphics_engine.act_6 () else
        if key = GdkKeysyms._8 then Graphics_engine.act_8 () else
        if key = GdkKeysyms._9 then Graphics_engine.act_9 () else
        if key = GdkKeysyms._d then Graphics_engine.dn () ;
      if Graphics_engine.get_init () then
        (Graphics_engine.display ();
         Graphics_engine.set_init ())
      else
        Graphics_engine.display ();
      area#swap_buffers ();
        true
    end);

    ignore (btn#connect#clicked ~callback:
        (fun () -> area#destroy (); w#destroy ();
                   Graphics_engine.set_init ()));
   ()

let exec_3d_obj () =
	(* PETAGE DU MOTEUR 3D *)
	Parser_obj.open_obj ();
	Parser_obj.put_color ();
    exec_glgtk ()

let exec_3d_inst () =
	(* 3D POUR LES PRESSES *)
	begin
	if ((Sys.command "./genperlin -save > /tmp/rand_map.bmp") = 0) then
		Refe.filename := "/tmp/rand_map.bmp"
	else
		Refe.filename := "carte.bmp"
	end;
  	(*Assist.view_img ();*)
	Refe.file_type := "img";
	Refe.step := 5;
	Pre.pre_trait ();
	Assist.rand_alt ();
    Refe.perso := false;
    exec_glgtk ()


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
    let btn_noclik = GButton.button
		~label:"Do not click"
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
    ignore (btn_noclik#connect#clicked
        ~callback:(fun () -> (exec_glgtk () )));

  	w#show ();
  	GMain.main ()

