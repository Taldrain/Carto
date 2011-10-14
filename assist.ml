(***************************************************************************)
(*		L'assistant pour les couleurs dans un premier temps 			   *)
(***************************************************************************)

type struct_alt = {
	lbl : GMisc.label;
	tbx : GEdit.entry
}
let list_lbltbx_struct = ref ([] : struct_alt list)

let first () =
	let win1 = GWindow.window 
		~title:"Assist first step" () 
		~width:800 
		~height:600 in
	win1#connect#destroy ~callback:GMain.quit;
	let vbox = GPack.vbox 
		~packing:win1#add () in
	let lbl = GMisc.label 
		~text:"Enter the colors for each level"
		~packing:vbox#add () in
	let separator = GMisc.separator `HORIZONTAL 
		~packing:vbox#add () in
	(* --------------------------------------- *)
	let scrolled_window = GBin.scrolled_window
		~border_width:10
		~hpolicy:`AUTOMATIC
		~vpolicy:`AUTOMATIC
		~packing:vbox#add () in
	let secvbox = GPack.vbox
		~packing:scrolled_window#add_with_viewport () in
	(* begin -- Generation des boutons en fonction de !nb_colors *)
	for i=1 to (Refe.get_nb_colors ()) do
		let temp = {
			lbl = GMisc.label
				~text:("Color "^(string_of_int i))
				~packing:secvbox#add ();
			tbx = GEdit.entry
				~max_length:4
				~packing:secvbox#add ()
		} in
			temp::(!list_lbltbx_struct)
	done;
	win1#show ()
	(* end -- Generation des boutons en fonction de !nb_colors *)
	let separator2 = GMisc.separator `HORIZONTAL 
		~packing:vbox#add () in
	(* --------------------------------------- *)
	let btn :

