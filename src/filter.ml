(* FILTER PART of THE PROJECT*)


(* image to matrix *)
let img_to_mat img = 
  let (w,h) = ((Sdlvideo.surface_info img).Sdlvideo.w, 
              (Sdlvideo.surface_info img).Sdlvideo.h) in
  let mat = (Array.make_matrix w h (0,0,0)) in 
    for x = 0 to (w-1) do
      for y = 0 to (h-1) do
        mat.(x).(y) <- Sdlvideo.get_pixel_color img x y;
      done;
    done;
  mat

(* ------------------------------- Filters ---------------------------------- *)

(* initialization of the matrix filter *)
let sobel1() = 
  let mat = Array.make_matrix 3 3 0 in
  mat.(0).(0) <- 1;
  mat.(1).(0) <- 0;
  mat.(2).(0) <-(-1);
  mat.(0).(1) <- 2;
  mat.(1).(1) <- 0;
  mat.(2).(1) <-(-2);
  mat.(0).(2) <- 1;
  mat.(1).(2) <- 0;
  mat.(2).(2) <-(-1);
  mat

let sobel2() = 
  let mat = Array.make_matrix 3 3 0 in
  mat.(0).(0) <- 1;
  mat.(1).(0) <- 2;
  mat.(2).(0) <- 1;
  mat.(0).(1) <- 0;
  mat.(1).(1) <- 0;
  mat.(2).(1) <- 0;
  mat.(0).(2) <-(-1);
  mat.(1).(2) <-(-2);
  mat.(2).(2) <-(-1);
  mat


(* ------------------------------------ ------------------------------------- *)

(* micro function that show a r/g/b element of a pixel *)
let atb_p img x y atb = 
  if atb = "r" then 
    let (r_out,_,_) = Sdlvideo.get_pixel_color img x y in r_out 
  else if atb = "g" then
    let (_,g_out,_) = Sdlvideo.get_pixel_color img x y in g_out
  else let (_,_,b_out) = Sdlvideo.get_pixel_color img x y in b_out

(* normalize function *)
let rec normalize pix = 
  let (r,g,b) = pix in
    match (r,g,b) with
    | (r,g,b) when r > 255 -> normalize (255,g,b); 
    | (r,g,b) when r < 0   -> normalize (0,g,b); 
    | (r,g,b) when g > 255 -> normalize (r,255,b); 
    | (r,g,b) when g < 0   -> normalize (r,0,b); 
    | (r,g,b) when b > 255 -> normalize (r,g,255); 
    | (r,g,b) when b < 0   -> normalize (r,g,0);
    | (r,g,b)              -> (r,g,b)

(* addition of pixel with all neighbours filtered (3x3) to make pixel filtered*)
let multi_pix_filter img x y filter =
 try 
  let r_out = (filter.(0).(0) * (atb_p img (x-1) (y-1) "r" ) + 
               filter.(1).(0) * (atb_p img (x) (y-1) "r" ) +
               filter.(2).(0) * (atb_p img (x+1) (y-1) "r" ) +
               filter.(0).(1) * (atb_p img (x-1) (y) "r" ) +
               filter.(1).(1) * (atb_p img (x) (y) "r" ) +
               filter.(2).(1) * (atb_p img (x+1) (y) "r" ) +
               filter.(0).(2) * (atb_p img (x-1) (y+1) "r" ) +
               filter.(1).(2) * (atb_p img (x) (y+1) "r" ) +
               filter.(2).(2) * (atb_p img (x+1) (y+1) "r" )) / 9 in 
  
  let g_out = (filter.(0).(0) * (atb_p img (x-1) (y-1) "g" ) + 
               filter.(1).(0) * (atb_p img (x) (y-1) "g" ) +
               filter.(2).(0) * (atb_p img (x+1) (y-1) "g" ) +
               filter.(0).(1) * (atb_p img (x-1) (y) "g" ) +
               filter.(1).(1) * (atb_p img (x) (y) "g" ) +
               filter.(2).(1) * (atb_p img (x+1) (y) "g" ) +
               filter.(0).(2) * (atb_p img (x-1) (y+1) "g" ) +
               filter.(1).(2) * (atb_p img (x) (y+1) "g" ) +
               filter.(2).(2) * (atb_p img (x+1) (y+1) "g" )) / 9 in 

  let b_out = (filter.(0).(0) * (atb_p img (x-1) (y-1) "b" ) + 
               filter.(1).(0) * (atb_p img (x) (y-1) "b" ) +
               filter.(2).(0) * (atb_p img (x+1) (y-1) "b" ) +
               filter.(0).(1) * (atb_p img (x-1) (y) "b" ) +
               filter.(1).(1) * (atb_p img (x) (y) "b" ) +
               filter.(2).(1) * (atb_p img (x+1) (y) "b" ) +
               filter.(0).(2) * (atb_p img (x-1) (y+1) "b" ) +
               filter.(1).(2) * (atb_p img (x) (y+1) "b" ) +
               filter.(2).(2) * (atb_p img (x+1) (y+1) "b" )) / 9 in 
  let pixel = (r_out, g_out, b_out) in
  normalize pixel;
 with _ -> Sdlvideo.get_pixel_color img x y

(* multiplication of filter and the matrix *)
let multi_mat_filter filter img = 
  let (w,h) = ((Sdlvideo.surface_info img).Sdlvideo.w, 
              (Sdlvideo.surface_info img).Sdlvideo.h) in
  let mat_out = Array.make_matrix w h (0,0,0) in
    for x = 0 to w - 1 do
     for y = 0 to h - 1 do
       mat_out.(x).(y) <- multi_pix_filter img x y filter;
     done;
    done;
  mat_out

(* matrix to image *)
let mat_to_img mat img = 
  let (w,h) = ((Sdlvideo.surface_info img).Sdlvideo.w, 
              (Sdlvideo.surface_info img).Sdlvideo.h) in
  let image = Sdlvideo.create_RGB_surface_format img [] w h in
    for x = 0 to (w-1) do
      for y = 0 to (h-1) do
        Sdlvideo.put_pixel_color image x y (mat.(x).(y)); 
      done;
    done;
  image

(* image to image simple filtered *)
let filtr_simpl_img img filter = 
  let mat_f = multi_mat_filter filter img in
    mat_to_img mat_f img

(* e1 = elt of mat1...    ef = sqrt (square e1 + square e2)  *)
let mat1_mat2 mat1 mat2 img = 
  let mat_f = 
    Array.make_matrix (Array.length mat1) (Array.length mat1.(0)) (0,0,0) in
      for x = 0 to (Array.length mat1 - 1) do
        for y = 0 to (Array.length mat1.(0) - 1) do
          let (r1,g1,b1) = mat1.(x).(y) in
          let (r2,g2,b2) = mat2.(x).(y) in
          mat_f.(x).(y) <- normalize ((int_of_float
            (Pervasives.sqrt (( (float_of_int r1) ** 2.) +.
            ((float_of_int r2) ** 2.)))),(int_of_float
            (Pervasives.sqrt (( (float_of_int g1) ** 2.) +.
            ((float_of_int g2) ** 2.)))),(int_of_float
            (Pervasives.sqrt (( (float_of_int b1) ** 2.) +.
            ((float_of_int b2) ** 2.)))));
        done;
      done;
  mat_f 

(* image to image double filtered *)
let filtr_doubl_img img filter1 filter2 = 
  let mat1 = multi_mat_filter filter1 img in
  let mat2 = multi_mat_filter filter2 img in
  mat_to_img (mat1_mat2 mat1 mat2 img) img 



(* ------------------------------------ ------------------------------------- *)

(* ---------------------------- Filter functions ---------------------------- *)

(* Soble filter   image -> image   *)
let sobel_filter img = 
  filtr_doubl_img img (sobel1()) (sobel2())

(* ------------------------------------ ------------------------------------- *)

(* END *)
