let coords list text = 
  let rcoolors = open_in "/(*chemin*)/text.txt" and
      wcoords = open_out "/(*chemin*)/colors_altitude.txt" in
    while List.length list <> 0 do
      let coolors = input_line rcoolors in
     	output_string wcoords (coolors^" "^(List.hd list));
	  List.tl list
    done;
   close_out wcoords;
   close_in rcoolors

    
