(* Parse des arguments entrés en parametres du programme *)

let annon_fun str =
  print_endline (str ^ " is not an option, check -help or --help.")

let sTS str =
  Sdlloader.load_image str

let save img str =
  Sdlvideo.save_BMP img str

let args =
  let s = " Apply the Sobel filter, on the .bmp" in
  let sc = " Apply the Sobel filter with color, on the .bmp" in
  let av1 = " Apply the Average filter with the precision of 3" in
  let av2 = " Apply the Average filter with the precision of 5" in
  let g = " Apply the Gauss filter" in
    (* Look at my indentation, my indentation is amazing... *)
  [("-sobel", Arg.String
     (fun str -> (save (Filter.sobel_filter_f (sTS str)) "sobel.bmp");
     ()), s);
   ("-sobelc", Arg.String
     (fun str -> (save (Filter.sobel_filter_f_color (sTS str)) "sobel_c.bmp");
     ()), sc);
   ("-moyen3", Arg.String
     (fun str -> (save (Filter.average1 (sTS str)) "average3.bmp");
     ()), av1);
   ("-moyen5", Arg.String
     (fun str -> (save (Filter.average2 (sTS str)) "average5.bmp");
     ()), av2);
   ("-gauss", Arg.String
     (fun str -> (save (Filter.gauss3_filter (sTS str)) "gauss.bmp");
     ()), g);
  ]

let parse () =
  if (Array.length Sys.argv) < 2 then
    Main.main ()
  else
    let usage_msg = "Usage: supermap (options) <.bmp>\nOptions are:" in
      Arg.parse (Arg.align args) annon_fun usage_msg

let _ = parse()
