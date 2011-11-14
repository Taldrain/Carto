(**************************************************************************)
(*    Carto - TopoTeam                                                    *)
(**************************************************************************)

let num = ref 0

(* BEGIN -- Main and various functions for the GTK interface *)
let quit () =
	GMain.quit ();
	exit 0

let exec_fst_treat btn =
	Assist.winstep ();
	btn#misc#set_sensitive true

let exec_assist () =
	Assist.winalt ()

let exec_brow win b =
	Browser.browser win;
	b#misc#set_sensitive true

let main () =

	ignore (GtkMain.Main.init ());

	(* Recuperation de la taille de l'ecran *)
	let screen = Gdk.Screen.default () in
	let screen_hei = Gdk.Screen.height ~screen:screen () in
	let screen_wid = Gdk.Screen.width ~screen:screen () in

  	let w = GWindow.window
		~title:"Carto TopoTeam" ()
		~width:screen_wid
		~height:screen_hei
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
	item1#set_submenu menufile;

	(* PANEL *)
	let pan = GPack.paned `HORIZONTAL
		~border_width:5
		~width:screen_wid
		~height:(screen_hei - 50)
		~packing:main_box#add () in
	(* Panel left *)
	let box = GPack.vbox
		~packing:pan#add1 () in
	let _lbl = GMisc.label
		~text:"Projet Carto -- Topo team"
		~packing:box#add () in
	
	let _lbl = GMisc.label
		~text:"\nEffectuer le pre traitement\navant de lancer l'assistant\n"
		~packing:box#add () in
	let btn_browse = GButton.button
		~label:"Browse"
		~packing:box#add () in
	let btn_pre_treat = GButton.button
		~label:"Execute"
		~packing:box#add () in
	let btn_assist = GButton.button
		~label:"Assist"
		~packing:box#add () in
	let btn_quit = GButton.button
		~label:"Quit"
	 	~packing:box#add () in

	(* Panel right *)
	let box = GPack.vbox
		~packing:pan#add2 () in
	(*let area = GlGtk.area []
		~packing:box#add () in*)
	let btn_test = GButton.button
		~label:"Test"
		~packing:box#add () in

	btn_pre_treat#misc#set_sensitive false;
	btn_assist#misc#set_sensitive false;

	(* --------- *)
	(* CALLBACKS *)
	(* --------- *)
	
	(*menu*)
	ignore (mf_open#connect#activate
		~callback:(fun () -> exec_brow w btn_pre_treat));
	ignore (mf_quit#connect#activate
		~callback:quit); 
	
	(*buttons*)
  	ignore (w#connect#destroy ~callback:GMain.quit);
	ignore (btn_browse#connect#clicked
		~callback:(fun () -> exec_brow w btn_pre_treat));
	ignore (btn_pre_treat#connect#clicked
		~callback:(fun () -> exec_fst_treat btn_assist));
	ignore (btn_assist#connect#clicked
		~callback:exec_assist);
	ignore (btn_quit#connect#clicked
		~callback:quit);

  	w#show ();
  	GMain.main ()

let _ = main ()
