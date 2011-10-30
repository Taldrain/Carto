(**************************************************************************)
(*    Carto - TopoTeam                                                    *)
(**************************************************************************)

(* BEGIN -- GLOBAL VARIABLES *)
(* END -- GLOBAL VARIABLES *)

let num = ref 0

(* BEGIN -- Main and various functions for the GTK interface *)
let quit () =
	GMain.quit ();
	exit 0

let exec_fst_treat () =
	if Refe.get_pos () == 1 then
		Assist.firstwin ()
	else
		()

let exec_assist () =
	if Refe.get_pos () == 2 then
		Assist.first ()
	else
		()

let main () =
	ignore (GtkMain.Main.init ());
  	let w = GWindow.window
		~title:"Carto TopoTeam" ()
		~width:200
		~height:400
		~position:`CENTER in
  	ignore (w#connect#destroy ~callback:GMain.quit);
	let vbox = GPack.vbox
		~packing:w#add () in
	let _lbl = GMisc.label
		~text:"Projet Carto -- Topo team"
		~packing:(vbox#pack ~expand:false ~fill:false) () in
	let _separator = GMisc.separator `HORIZONTAL
		~packing:(vbox#pack ~expand:false ~fill:false) () in
	let lbl = ref (GMisc.label
		~text:"\nEffectuer le pre traitement\navant de lancer l'assistant\n"
		~packing:(vbox#pack ~expand:false ~fill:false) ()) in
	let _lbl2 = GMisc.label
		~text:(Refe.get_filename ())
		~packing:(vbox#pack ~expand:false ~fill:false) () in
    let btn_browse = GButton.button
		~label:"Browse"
		~packing:(vbox#pack ~padding:5) () in
	ignore (btn_browse#connect#clicked ~callback:(Browser.browser w));
	let btn_pre_trait = GButton.button
		~label:"Execute"
		~packing:(vbox#pack ~padding:5) () in
	ignore (btn_pre_trait#connect#clicked
		~callback:(exec_fst_treat));
	let btn_assist = GButton.button
		~label:"Assist"
		~packing:(vbox#pack ~padding:5) () in
	ignore (btn_assist#connect#clicked ~callback:(exec_assist));
	let btn_quit = GButton.button
		~label:"Quit"
		~packing:(vbox#pack ~padding:5) () in
	ignore (btn_quit#connect#clicked ~callback:((fun _ -> !lbl#set_text "Test")));
	(*ignore (btn_quit#connect#clicked ~callback:(quit));*)
  	w#show ();
  	GMain.main ()
(* BEGIN -- Main and various functions for the GTK interface *)

let _ = main ()
