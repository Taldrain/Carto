(* BEGIN -- Functions for the pre traitement *)
(* PROJET CARTOGRAPHIE  *)
(* pre-traitement & quadrillage *)


(* FONCTIONS *)

(* Dimensions d'une image *)
let get_dims img =
  begin
    Refe.w := (Sdlvideo.surface_info img).Sdlvideo.w;
    Refe.h := (Sdlvideo.surface_info img).Sdlvideo.h;
  end

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

(* affiche la surface img sur la surface de destination dst
   (normalement l'écran) *)
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

(* fonction contour *)
let contour image image2 =
  begin
    let right = ref (0,0,0)
    and down = ref (0,0,0)
    and center = ref (0,0,0)
    and listcolor = ref [] in
      for x=0 to (Refe.get_w())-1 do
	for y=0 to (Refe.get_h())-1 do
	  center := Sdlvideo.get_pixel_color image x y;
	  right := Sdlvideo.get_pixel_color image (x+1) y;
	  down := Sdlvideo.get_pixel_color image x (y+1);
	  if (!center <> !right &&  x < (Refe.get_w())-1
	      || !center <> !down &&  y < (Refe.get_h())-1 ) then
	    Sdlvideo.put_pixel_color image2 x y (0,0,0) else
	      Sdlvideo.put_pixel_color image2 x y !center;
	  if (!center <> !right || !center <> !down)
	  then listcolor := p_list !listcolor !center
	  else() ;
	done;
      done;
      Refe.li := !listcolor;
      list_to_text !listcolor;
  end


(* quadrillage horizontal *)
 let contour_hor image interv_y =
   begin
       for y=0 to (Refe.get_h())-1 do
	 if y mod interv_y = 0 then
	   for x=0 to (Refe.get_w())-1 do
	   Sdlvideo.put_pixel_color image x y (0,0,0);
	   done;
       done;
   end

(* quadrillage vertical *)
 let contour_ver image interv_x =
   begin
     for x=0 to (Refe.get_w())-1 do
       if x mod interv_x = 0 then
	 for y=0 to (Refe.get_h())-1 do
	   Sdlvideo.put_pixel_color image x y (0,0,0);
	 done;
     done;
   end

(* quadrillage diagonal gauchehaut-droitebas *)
 let contour_diag1 image interv =
   let rec con_d img x y inc_x inc_y y_finish =
     match (x, y, inc_x, inc_y, y_finish) with
	 (x, y, inc_x, inc_y, y_finish) when inc_x > (Refe.get_w())-1 -> ()
       | (x, y, inc_x, inc_y, y_finish) when inc_y > (Refe.get_h())-1
	   -> con_d img 0 0 0 0 true
       | (x, y, inc_x, inc_y, y_finish) when  x >= (Refe.get_w())-1 || y >= (Refe.get_h())-1
	   -> if inc_x < (Refe.get_w())-1 && y_finish = true then
	     con_d img inc_x inc_y (inc_x + interv) 0 true else
	       if inc_x = 0 && y_finish = false then
		 con_d img inc_x inc_y 0 (inc_y + interv) false
       | (x, y, inc_x, inc_y, y_finish)
	 -> Sdlvideo.put_pixel_color img x y (0,0,0);
	   con_d img (x+1) (y+1) inc_x inc_y y_finish
   in con_d image 0 0 0 0 false

(*quadrillage diagonal gauchebas-droitehaut *)
(* let contour_diag2 image interv =
   let rec con_d img x y inc_x inc_y  =
     match (x, y, inc_x, inc_y) with
	 (x, y, inc_x, inc_y) when inc_x >= (Refe.get_w())-1 -> ()
       | (x, y, inc_x, inc_y) when inc_y >= (Refe.get_h())-1 && x > (Refe.get_w())-1
	   -> con_d img inc_x inc_y (inc_x+interv) inc_y
       | (x, y, inc_x, inc_y) when  inc_y < (Refe.get_h())-1 && (y <= 0)
	   ->  con_d img 0 inc_y 0 (inc_y+interv)
       | (x, y, inc_x, inc_y)
	 -> Sdlvideo.put_pixel_color img x y (0,0,0);
	   con_d img (x+1) (y-1) inc_x inc_y
   in con_d image 0 0 0 interv
*)



(* creation de la matrice ayant les points des intersections de la
   carte *)


 let rec map_to_mat x y intx inty = match (intx, inty) with
   | (intx, inty) when intx > ((Refe.get_w())) ->
       map_to_mat 0 (y+1) 0 (inty + (Refe.get_step()))
   | (intx, inty) when inty > ((Refe.get_h())) -> ()
   | (intx, inty) -> Array.set (Refe.get_matrice()).(x) (y) (inty,intx); map_to_mat
       (x+1) y (intx+((Refe.get_step()))) inty



 let matXY_to_matRGB img =
   begin
     for y = 0 to (((Refe.get_h()))/((Refe.get_step()))) do
       for x = 0 to (((Refe.get_w()))/((Refe.get_step()))) do
 	 let (a,b) = (Refe.get_matrice()).(x).(y) in
           Array.set (Refe.get_matrice_rgb()).(x) (y)
 	     ((x,y),(Sdlvideo.get_pixel_color img a b))
       done;
     done;
   end




(* main *)
let pre_trait () =
  begin
    (* Nous voulons 1 argument *)
    if (Refe.get_filename()) == "" then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image (Refe.get_filename ()) in
      (* On récupère les dimensions *)
      get_dims img;
      let img2 = Sdlvideo.create_RGB_surface_format
	img [] (Refe.get_w()) (Refe.get_h()) in
	(* On crée la surface d'affichage *)
      let display = Sdlvideo.set_video_mode (Refe.get_w()) (Refe.get_h()) [] in
	(* on affiche l'image avant traitement*)
	show img display;
	(* on attend une touche *)
	wait_key();
	(* on appelle la fonction de pretraitement*)
	contour img img2;
	show img2 display;
	wait_key ();
	(* fonction de quadrillage *)
	contour_hor img2 (Refe.get_step());
	contour_ver img2 (Refe.get_step());
	contour_diag1 img2 (Refe.get_step());
	map_to_mat 0 0 0 0;
	matXY_to_matRGB img;
	(*  contour_diag2 img2 interv; *)
	(* on affiche l'image apres traitement*)
	show img2 display;
	(* enregistrement *)
	Sdlvideo.save_BMP img2 "out.bmp";
	(* on attend une touche *)
	wait_key ();
	(* on quitte *)
	Sdl.quit ();
	Refe.pos := 2
  end
(* END -- Functions for the pre traitement *)
