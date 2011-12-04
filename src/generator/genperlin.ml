(* graphic test *)

open Perlin_stub

let alpha_ref = ref 2.0
let beta_ref  = ref 2.0
let octave    = ref 10
let save_flag = ref false
let seed      = ref 0
let povcolor  = ref false
let grey      = ref false

let make_image w h =
  Bigarray.Array3.create
    Bigarray.int8_unsigned
    Bigarray.c_layout w h 3

let set_pixel image x y (r,g,b) =
  image.{x,y,0} <- r;
  image.{x,y,1} <- g;
  image.{x,y,2} <- b

let flatcolor3 f =
  let g = int_of_float (896. *. (abs_float f)) mod 1024 in
    if g < 96 then
      (0,0,255)
    else if g < 256 then
      (0,63,0)
    else if g < 320 then
      (0,127,0)
    else if g < 384 then
      (0,191,0)
    else if g < 448 then
      (0,255,0)
    else if g < 512 then
      (0,255,127)
    else if g < 576 then
      (127,255,127)
    else if g < 768 then
      (191,255,191)
    else
      (255,255,255)

let povColor f =
  let g = ((int_of_float (16.01 *. (abs_float f)) ) * 16 ) mod 256 in
    (g,g,g)

let gradient f =
  let g = min (int_of_float (250. *. (abs_float f))) 255 in
    (g,g,g)

let gencolor f =
  if !povcolor then
    povColor f
  else if !grey then
    gradient f
  else
    flatcolor3 f

let flatcolor f =
  let (r,g,b) = flatcolor3 f in
    Graphics.rgb r g b

let float2color f =
  let i = int_of_float (256. *. (abs_float f)) mod 256 in
    Graphics.rgb i i i

let affiche_image img w h =
  begin
    Graphics.clear_graph ();
    for j = (h-1) downto 0 do
      for i = 0 to (w-1) do
	Graphics.set_color
	  (Graphics.rgb img.{i,j,0} img.{i,j,1} img.{i,j,2});
	Graphics.plot i j ;
      done;
    done;
    ignore (Graphics.read_key ())
  end

let gen_image s =
  let coord2float x =
    (float x)/.(float s)
  in
  function f ->
    begin
      let img = make_image s s in
	for j = 0 to s - 1 do
	  for i = 0 to (s-1) do
	    set_pixel img i j
	      (gencolor (f (coord2float i) (coord2float j)));
	  done;
	done;
	if !save_flag then
	  save_bmp img;
	(*affiche_image img s s;*)
    end

let al = Arg.align [
  ("-alpha", Arg.Set_float alpha_ref, " Alpha parameter");
  ("-beta" , Arg.Set_float beta_ref, " Beta parameter");
  ("-octave", Arg.Set_int octave, " Number of octaves");
  ("-save", Arg.Set save_flag, " Output bmp to stdout");
  ("-seed", Arg.Set_int seed, " Initial random seed");
  ("-povcolor", Arg.Set povcolor, " Use color for POVray HF");
  ("-grey", Arg.Set grey, " Smooth grey level")
]

let _ =
  Arg.parse al (fun _ -> ()) "Perlin noise map generator:";
  (*Graphics.open_graph " 512x512";*)
  init_rand !seed;
  gen_image 512
    (fun x y -> c_perlin x y  !alpha_ref !beta_ref !octave) ;
  exit 0
