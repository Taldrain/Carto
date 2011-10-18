(* BEGIN -- Functions for the browser *)
let default d = function
	| None -> d
	| Some v -> v

let all_files () =
	let f = GFile.filter ~name:"All" () in
	f#add_pattern "*";
	f

let is_string_prefix s1 s2 =
	let l1 = String.length s1 in
	let l2 = String.length s2 in
	l1 <= l2 && s1 = String.sub s2 0 l1

let image_filter () =
	let f = GFile.filter ~name:"Images" () in
	f#add_custom [ `MIME_TYPE ]
	(fun info -> let mime = List.assoc `MIME_TYPE info in
		is_string_prefix "image/" mime);
	f

let browser parent () =
	let dialog = GWindow.file_chooser_dialog
		~action: `OPEN
		~title: "Open File"
		~parent () in
	dialog#add_button_stock `CANCEL `CANCEL;
	dialog#add_select_button_stock `OPEN `OPEN;
	dialog#add_filter (image_filter ());
	dialog#add_filter (all_files ());
	begin match dialog#run () with
	| `OPEN -> Refe.filename := (default "<none>" dialog#filename);
				prerr_endline (Refe.get_filename ())
	| `DELETE_EVENT | `CANCEL -> ()
	end;
	dialog#destroy ()
(* END -- Funcmions for the browser *)

