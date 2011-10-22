let altitude pix = 
  let rec alt pi li = match li with
  | [] -> 0
  | e::li when e.Refe.rgb = pi -> e.Refe.alt
  | e::_ -> alt pi li
  in alt pix (Refe.get_list_alt())

let get_alt =
  for y = 0 to (Refe.get_h())/( Refe.get_step()) do
	for x = 0 to (Refe.get_w())/(Refe.get_step()) do
		let ((d,e), b) = (Refe.get_matrice_rgb()).(x).(y) in
		let alti = altitude b in
		Array.set (Refe.get_matrice_ret()).(x) (y) ((d, e, alti),b)	
	done;	
	done
