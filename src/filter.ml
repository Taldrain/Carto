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
(*let multi_pix_filter img x y filter = 
  let r_out = 
    
  let g_out = ref 0 in
  let b_out = ref 0 in
  let 
*)
(* multiplication of filter and the matrix *)



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

(* ------------------------------------ ------------------------------------- *)

(* END *)
