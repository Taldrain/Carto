(**************************************************************************)
(*    Carto - TopoTeam                                                    *)
(**************************************************************************)

(* BEGIN -- GLOBAL VARIABLES *)
(* END -- GLOBAL VARIABLES *)

(* BEGIN -- Main and various functions for the GTK interface *)
let quit () =
	exit 0

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

	let _lbl = GMisc.label
		~text:"\nEffectuer le pre traitement\navant de lancer l'assistant\n"
		~packing:(vbox#pack ~expand:false ~fill:false) () in

    let btn_browse = GButton.button
			~label:"Browse"
			~packing:(vbox#pack ~padding:5) () in
	ignore (btn_browse#connect#clicked ~callback:(Browser.browser w));
	let btn_pre_trait = GButton.button
			~label:"Execute"
			~packing:(vbox#pack ~padding:5) () in
	ignore (btn_pre_trait#connect#clicked ~callback:Pre.pre_trait);
	let btn_assist = GButton.button
		~label:"Assist"
		~packing:(vbox#pack ~padding:5) () in
	ignore (btn_assist#connect#clicked ~callback:(Assist.first));
	let btn_quit = GButton.button
		~label:"Quit"
		~packing:(vbox#pack ~padding:5) () in
	ignore (btn_quit#connect#clicked ~callback:(quit));
  	w#show ();
  	GMain.main ()
(* BEGIN -- Main and various functions for the GTK interface *)

let _ = main ()
