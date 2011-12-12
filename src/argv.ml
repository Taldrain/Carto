(* Parsing of arguments *)

let annon_fun str =
  print_endline ("'"^str^"'"^" is not an option, check -help or --help.")

let sTS str =
  Sdlloader.load_image str

let save img str =
  Sdlvideo.save_BMP img str

let args =
  let s = " Apply the Sobel filter, on the .bmp" in
  let sc = " Apply the Sobel filter with color, on the .bmp" in
  let av3 = " Apply the Average filter with the precision of 3" in
  let av5 = " Apply the Average filter with the precision of 5" in
  let g = " Apply the Gauss filter" in
  let med3 = " Apply the Median filter with a 3x3 matrix" in
  let med5 = " Apply the Median filter with a 5x5 matrix" in
  let grey = " Apply the Grey filter" in
  let inst_3d = " Instant 3d on a randomly generate map" in
  let v = " Print version and exit" in
    (* Look at my indentation, my indentation is Amazing... *)
  [("-sobel", Arg.String
     (fun str -> (save (Filter.sobel_filter_f (sTS str)) "sobel.bmp");
     ()), s);
   ("-sobelc", Arg.String
     (fun str -> (save (Filter.sobel_filter_f_color (sTS str)) "sobel_c.bmp");
     ()), sc);
   ("-moyen3", Arg.String
     (fun str -> (save (Filter.average1 (sTS str)) "average3.bmp");
     ()), av3);
   ("-moyen5", Arg.String
     (fun str -> (save (Filter.average2 (sTS str)) "average5.bmp");
     ()), av5);
   ("-gauss", Arg.String
     (fun str -> (save (Filter.gauss3_filter (sTS str)) "gauss.bmp");
     ()), g);
   ("-median3", Arg.String
     (fun str -> (save (Filter.median_filtr3 (sTS str)) "median3.bmp");
     ()), med3);
   ("-median5", Arg.String
     (fun str -> (save (Filter.median_filtr5 (sTS str)) "median5.bmp");
     ()), med5);
   ("-grey", Arg.String
     (fun str -> (save (Filter.img_to_grey (sTS str)) "grey.bmp");
     ()), grey);
   ("-r3d", Arg.Unit
     (fun () -> (Main.exec_3d_inst ());
     ()), inst_3d);
   ("-v", Arg.Unit
     (fun () -> (print_endline (Refe.gVersion));
     ()), v);
  ]

let parse () =
  if (Array.length Sys.argv) < 2 then
    Main.main ()
  else
    let usage_msg = "Usage: supermap (options) <.bmp>\nOptions are:" in
      Arg.parse (Arg.align args) annon_fun usage_msg

let _ = parse()
