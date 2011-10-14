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
		~width:1024 
		~height:768 in
  	w#connect#destroy ~callback:GMain.quit;

	let vbox = GPack.vbox 
		~packing:w#add () in
	let lbl = GMisc.label 
			~text:"Projet Carto -- Topo team" 
			~packing:vbox#add () in
	let separator = GMisc.separator `HORIZONTAL 
		~packing:vbox#add () in

	let img = GMisc.image 
			~file:(Refe.get_filename ()) 
			~packing:(vbox#pack ~padding:5) () in
    let btn_browse = GButton.button 
			~label:"Browse" 
			~packing:(vbox#pack ~padding:5) () in
	btn_browse#connect#clicked ~callback:(Browser.browser w);
	let btn_pre_trait = GButton.button 
			~label:"Execute" 
			~packing:(vbox#pack ~padding:5) () in
	btn_pre_trait#connect#clicked ~callback:Pre.pre_trait;
	let btn_quit = GButton.button 
		~label:"Quit" 
		~packing:(vbox#pack ~padding:5) () in
	btn_quit#connect#clicked ~callback:(quit);
	let btn_assist = GButton.button
		~label:"Assist"
		~packing:(vbox#pack ~padding:5) () in
	btn_assist#connect#clicked ~callback:(Assist.first);
  	w#show ();
  	GMain.main ()
(* BEGIN -- Main and various functions for the GTK interface *)

let _ = main ()
