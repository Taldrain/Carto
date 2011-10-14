(* BEGIN -- Functions for the pre traitement *)
(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
 
(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()
 
(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

(* fonction qui analyse si l'element est dans la liste, si oui
   le rajoute dans celle ci *)

let rec p_list li pix = 
  begin
    match li with
	[] -> [pix]
      | e::li when e<>pix -> e::(p_list li pix)
      | _ -> li
  end

(* (int*int*int) -> string *)
let str_of_tri tr = let (a,b,c) = tr in 
  "("^string_of_int(a)^","^string_of_int(b)^","^string_of_int(c)^")\n"

(* (int*int*int)list -> string list *)
let rec stringList li = match li with
    [] -> []
  | e::li -> (str_of_tri e)::(stringList li) 

(* fonction qui ecrit une liste dans un .txt, le creer si 
   inexistant et le ferme *)

let list_to_text li = 
  begin
    let f = open_out "InfoCarto.txt" in 
    let stringLi = stringList li in 
    List.iter (output_string f) stringLi;
      close_out f;
  end

let contour image image2= 
  begin
    let right = ref (0,0,0)
    and (w,h) = get_dims image
    and down = ref (0,0,0)
    and center = ref (0,0,0)
    and listColor = ref [] in
      for x=0 to w-1 do
	for y=0 to h-1 do
	  center := Sdlvideo.get_pixel_color image x y;
	  right := Sdlvideo.get_pixel_color image (x+1) y;
	  down := Sdlvideo.get_pixel_color image x (y+1);
	  if (!center <> !right || !center <> !down)
	  then Sdlvideo.put_pixel_color image2 x y (0,0,0) 
	  else Sdlvideo.put_pixel_color image2 x y !center;
	  if (!center <> !right || !center <> !down) 
	  then listColor := p_list !listColor !center
	  else() ;
	done;
      done;
      list_to_text !listColor;
  end
    
(* main *)
let pre_trait () =
  begin
    (* Nous voulons 1 argument *)
    if ((Refe.get_filename ()) = "") then
      failwith (Refe.get_filename ());
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image (Refe.get_filename ()) in
      (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    let img2 =	Sdlvideo.create_RGB_surface_format img [] w h in
      (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h  [`DOUBLEBUF] in
      (* sans doublebuffering pour voir la difference *)
    let display2 = Sdlvideo.set_video_mode w h [] in
      (* on affiche l'image avant traitement*)
      (*show img display;*)
      (* on attend une touche *)
      (*wait_key();*)
      (*show img display2;*)
      (*wait_key();*)
      (* on appel la fonction de traitement*)
      contour img img2;
      (* on affiche l'image apres traitement*)
      show img2 display;
      wait_key ();
      (* enregistrement *)
      Sdlvideo.save_BMP img2 "out.bmp";
      (* test ecriture txt*)
      (* let f = open_out "test.txt" in
	output_string*)
      (* on quitte *)
      (*exit 0*)
	  Sdl.quit ()
  end
(* END -- Functions for the pre traitement *)
