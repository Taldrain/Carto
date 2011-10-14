(**************************************************************************)
(*    Carto - TopoTeam                                                    *)
(**************************************************************************)

(* BEGIN -- GLOBAL VARIABLES *)
let nb_colors = ref 3
type struct_alt = {
	lbl : GMisc.label;
	tbx : GEdit.entry
}
let list_lbltbx_struct = ref ([] : struct_alt list)
(* END -- GLOBAL VARIABLES *)

(* BEGIN -- Main and various functions for the GTK interface *)
let quit () = 
	exit 0;
	()

let main () =
	GtkMain.Main.init ();
  	let w = GWindow.window ~title:"Carto TopoTeam" () ~width:1024 ~height:768 in
  	w#connect#destroy ~callback:GMain.quit;

	let vbox = GPack.vbox ~packing:w#add () in
	let lbl = 
		GMisc.label ~text:"Projet Carto -- Topo team" ~packing:vbox#add () in
	let separator = GMisc.separator `HORIZONTAL ~packing:vbox#add () in
	(* example of using tbx
	let entry1 = GEdit.entry ~max_length:4 ~packing:vbox#add () in
		entry1#set_text "Nb1";
		entry1#select_region ~start:0 ~stop:entry1#text_length ;*)

	(* begin -- Generation des boutons en fonction de !nb_colors *)
	for i=1 to !nb_colors do
		let boxy = GPack.hbox ~packing:vbox#add () in
		let temp = {
			lbl = GMisc.label
				~text:("Color "^(string_of_int i))
				~packing:boxy#add ();
			tbx = GEdit.entry
				~max_length:4
				~packing:boxy#add ()
		} in
			temp::(!list_lbltbx_struct)
	done;
	(* end -- Generation des boutons en fonction de !nb_colors *)
	let img = GMisc.image ~file:(Refe.get_filename ()) ~packing:(vbox#pack ~padding:5) () in
    let btn_browse =
		GButton.button ~label:"Browse" ~packing:(vbox#pack ~padding:5) () in
	btn_browse#connect#clicked ~callback:(Browser.browser w);
	let btn_pre_trait =
		GButton.button ~label:"Execute" ~packing:(vbox#pack ~padding:5) () in
	btn_pre_trait#connect#clicked ~callback:Pre.pre_trait;
	let btn_quit =
		GButton.button ~label:"Quit" ~packing:(vbox#pack ~padding:5) () in
	btn_quit#connect#clicked ~callback:(quit);

  	w#show ();
  	GMain.main ()
(* BEGIN -- Main and various functions for the GTK interface *)

let _ = main ()
