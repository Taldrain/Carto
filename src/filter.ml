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

(* initialization of the matrix filter *)

(* micro function that show a r/g/b element of a pixel *)
let atb_p img x y atb = 
  if atb = "r" then 
    let (r_out,_,_) = Sdlvideo.get_pixel_color img x y in r_out 
  else if atb = "g" then
    let (_,g_out,_) = Sdlvideo.get_pixel_color img x y in g_out
  else let (_,_,b_out) = Sdlvideo.get_pixel_color img x y in b_out



(* addition of pixel with all neighbours filtered (3x3) to make pixel filtered*)
let multi_pix_filter img x y filter = 
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
  pixel

(* multiplication of filter and the matrix *)
let multi_mat_filter mat filter img = 
  let mat_out = Array.make_matrix (Array.length mat) (Array.length mat.(0)) (0,0,0) in
    for x = 0 to (Array.length mat - 1) do
     for y = 0 to (Array.length mat.(0) - 1) do
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

(* image to image filtered *)
let filtr_img img filter = 
  let mat_f = multi_mat_filter (img_to_mat img) filter img in
    mat_to_img mat_f img

(* ------------------------------------ ------------------------------------- *)

(* END *)
