(**************************************************************************)
(*    Carto - TopoTeam                                                    *)
(**************************************************************************)

(* BEGIN -- GLOBAL VARIABLES *)
(* END -- GLOBAL VARIABLES *)

(* BEGIN -- Main and various functions for the GTK interface *)
let quit () = 
	exit 0;
	()

let main () =
	GtkMain.Main.init ();
  	let w = GWindow.window 
		~title:"Carto TopoTeam" () 
		~width:200 
		~height:400 in
  	w#connect#destroy ~callback:GMain.quit;

	let box = GPack.fixed 
		~packing:w#add () in
	let lbl = GMisc.label 
			~text:"Projet Carto -- Topo team" 
			~packing:box#add () in
	let separator = GMisc.separator `HORIZONTAL 
		~packing:box#add () in

	let img = GMisc.image 
			~file:(Refe.get_filename ()) 
			~packing:box#add () in
    let btn_browse = GButton.button 
			~label:"Browse" 
			~packing:box#add () in
	btn_browse#connect#clicked ~callback:(Browser.browser w);
	let btn_pre_trait = GButton.button 
			~label:"Execute" 
			~packing:box#add () in
	btn_pre_trait#connect#clicked ~callback:Pre.pre_trait;
	let btn_quit = GButton.button 
		~label:"Quit" 
		~packing:box#add () in
	btn_quit#connect#clicked ~callback:(quit);
	let btn_assist = GButton.button
		~label:"Assist"
		~packing:box#add () in
	btn_assist#connect#clicked ~callback:(Assist.first);

	(* Placement des elements sur la fenetre *)
	(* box#put lbl 100 5; marche pas !!! *)
	(* Fin placement des elements sur la fenetre *)



  	w#show ();
  	GMain.main ()
(* BEGIN -- Main and various functions for the GTK interface *)

let _ = main ()
