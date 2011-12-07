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

let sobelv2_1() = 
  let mat = Array.make_matrix 3 3 0 in
  mat.(0).(0) <- (-1);
  mat.(1).(0) <-   0;
  mat.(2).(0) <-   1;
  mat.(0).(1) <- (-2);
  mat.(1).(1) <-   0;
  mat.(2).(1) <-   2;
  mat.(0).(2) <- (-1);
  mat.(1).(2) <-   0;
  mat.(2).(2) <-   1;
  mat

let sobelv2_2() = 
  let mat = Array.make_matrix 3 3 0 in
  mat.(0).(0) <- (-1);
  mat.(1).(0) <- (-2);
  mat.(2).(0) <- (-1);
  mat.(0).(1) <-   0;
  mat.(1).(1) <-   0;
  mat.(2).(1) <-   0;
  mat.(0).(2) <-   1;
  mat.(1).(2) <-   2;
  mat.(2).(2) <-   1;
  mat

let average3() =
  let mat = Array.make_matrix 3 3 0 in
  mat.(0).(0) <- 1;
  mat.(1).(0) <- 1;
  mat.(2).(0) <- 1;
  mat.(0).(1) <- 1;
  mat.(1).(1) <- 1;
  mat.(2).(1) <- 1;
  mat.(0).(2) <- 1;
  mat.(1).(2) <- 1;
  mat.(2).(2) <- 1;
  mat

let average5() = 
  let mat = Array.make_matrix 5 5 0 in
  mat.(0).(0) <- 1;
  mat.(1).(0) <- 1;
  mat.(2).(0) <- 1;
  mat.(3).(0) <- 1;
  mat.(4).(0) <- 1;
  mat.(0).(1) <- 1;
  mat.(1).(1) <- 1;
  mat.(2).(1) <- 1;
  mat.(3).(1) <- 1;
  mat.(4).(1) <- 1;
  mat.(0).(2) <- 1;
  mat.(1).(2) <- 1;
  mat.(2).(2) <- 1;
  mat.(3).(2) <- 1;
  mat.(4).(2) <- 1;
  mat.(0).(3) <- 1;
  mat.(1).(3) <- 1;
  mat.(2).(3) <- 1;
  mat.(3).(3) <- 1;
  mat.(4).(3) <- 1;
  mat.(0).(4) <- 1;
  mat.(1).(4) <- 1;
  mat.(2).(4) <- 1;
  mat.(3).(4) <- 1;
  mat.(4).(4) <- 1;
  mat 

(* ------------------------------------ ------------------------------------- *)

(* -------------------------- Pre filter functions -------------------------- *)


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



(* function needed for the next function 3x3 *)    
let mpf3 img x y filter color =
  try 
  (filter.(0).(0) * (atb_p img (x-1) (y-1) color ) + 
   filter.(1).(0) * (atb_p img (x  ) (y-1) color ) +
   filter.(2).(0) * (atb_p img (x+1) (y-1) color ) +
   filter.(0).(1) * (atb_p img (x-1) (y  ) color ) +
   filter.(1).(1) * (atb_p img (x  ) (y  ) color ) +
   filter.(2).(1) * (atb_p img (x+1) (y  ) color ) +
   filter.(0).(2) * (atb_p img (x-1) (y+1) color ) +
   filter.(1).(2) * (atb_p img (x  ) (y+1) color ) +
   filter.(2).(2) * (atb_p img (x+1) (y+1) color )) / 9  
  with _ -> 0

(* addition of pixel with all neighbours filtered (3x3) to make pixel filtered*)
let multi3_pix_filter img x y filter =
  let pixel = (mpf3 img x y filter "r",mpf3 img x y filter "g" 
                                      , mpf3 img x y filter "b") in
    normalize pixel

    
(* function needed for the next function 5x5 *)
let mpf5 img x y filter color =
 try
 (filter.(0).(0) * (atb_p img (x-2) (y-2) color ) +
  filter.(1).(0) * (atb_p img (x-1) (y-2) color ) +
  filter.(2).(0) * (atb_p img (x  ) (y-2) color ) +
  filter.(3).(0) * (atb_p img (x+1) (y-2) color ) +
  filter.(4).(0) * (atb_p img (x+2) (y-2) color ) +
  filter.(0).(1) * (atb_p img (x-2) (y-1) color ) +
  filter.(1).(1) * (atb_p img (x-1) (y-1) color ) +
  filter.(2).(1) * (atb_p img (x  ) (y-1) color ) +
  filter.(3).(1) * (atb_p img (x+1) (y-1) color ) +
  filter.(4).(1) * (atb_p img (x+2) (y-1) color ) +
  filter.(0).(2) * (atb_p img (x-2) (y  ) color ) +
  filter.(1).(2) * (atb_p img (x-1) (y  ) color ) +
  filter.(2).(2) * (atb_p img (x  ) (y  ) color ) +
  filter.(3).(2) * (atb_p img (x+1) (y  ) color ) +
  filter.(4).(2) * (atb_p img (x+2) (y  ) color ) +
  filter.(0).(3) * (atb_p img (x-2) (y+1) color ) +
  filter.(1).(3) * (atb_p img (x-1) (y+1) color ) +
  filter.(2).(3) * (atb_p img (x  ) (y+1) color ) +
  filter.(3).(3) * (atb_p img (x+1) (y+1) color ) +
  filter.(4).(3) * (atb_p img (x+2) (y+1) color ) +
  filter.(0).(4) * (atb_p img (x-2) (y+2) color ) +
  filter.(1).(4) * (atb_p img (x-1) (y+2) color ) +
  filter.(2).(4) * (atb_p img (x  ) (y+2) color ) +
  filter.(3).(4) * (atb_p img (x+1) (y+2) color ) +
  filter.(4).(4) * (atb_p img (x+2) (y+2) color )) / 25
 with _ -> 0


(* addition of pixel with all neighbours filtered (5x5) to make pixel filtered*)
let multi5_pix_filter img x y filter = 
  let pixel = (mpf5 img x y filter "r",mpf5 img x y filter "g"
                                      , mpf5 img x y filter "b") in
    normalize pixel


(* multiplication of filter and the matrix (3x3) *)
let multi_mat_filter filter img dim = 
  let (w,h) = ((Sdlvideo.surface_info img).Sdlvideo.w, 
              (Sdlvideo.surface_info img).Sdlvideo.h) in
  let mat_out = Array.make_matrix w h (0,0,0) in
    for x = 0 to w - 1 do
     for y = 0 to h - 1 do
       if dim = 3 then
         mat_out.(x).(y) <- multi3_pix_filter img x y filter
       else  mat_out.(x).(y) <- multi5_pix_filter img x y filter;
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

(* image to image simple filtered *)
let filtr_simpl_img img filter dim = 
  let mat_f = multi_mat_filter filter img dim in
    mat_to_img mat_f img

(* image to image double filtered *)
let filtr_doubl_img img filter1 filter2 dim = 
  let mat1 = multi_mat_filter filter1 img dim in
  let mat2 = multi_mat_filter filter2 img dim in
    mat1_mat2 mat1 mat2 img


(* transform to white pixel all non-black pixels    mat -> image *)
let mat_edge_to_white mat_i img =
  let mat_f = 
   Array.make_matrix (Array.length mat_i) (Array.length mat_i.(0)) (0,0,0) in 
    for x = 0 to (Array.length mat_i - 1) do 
      for y = 0 to (Array.length mat_i.(0) - 1) do
       if mat_i.(x).(y) <> (0,0,0) then
         mat_f.(x).(y) <- (255,255,255)
       else mat_f.(x).(y) <- mat_i.(x).(y);
      done;
     done;
     mat_to_img mat_f img

(* ------------------------------------ ------------------------------------- *)

(* ---------------------------- Filter functions ---------------------------- *)

(* Soble filter   image -> image   *)
let sobel_filter img = 
  mat_to_img (filtr_doubl_img img (sobel1()) (sobel2()) 3) img

let sobel_filter2 img = 
  mat_to_img (filtr_doubl_img img (sobelv2_1()) (sobelv2_2()) 3) img

(* complete sobel filter with colored edges*) 
let sobel_filter_f img = 
  mat_to_img (mat1_mat2 (filtr_doubl_img img (sobel1()) (sobel2()) 3)
                    (filtr_doubl_img img (sobelv2_1()) (sobelv2_2()) 3) img) img

(* sobel filter with white edges *)
let sobel_filter_f_color img = 
  mat_edge_to_white (mat1_mat2 (filtr_doubl_img img (sobel1()) (sobel2()) 3)
                    (filtr_doubl_img img (sobelv2_1()) (sobelv2_2()) 3) img) img

(* average3 filter *)
let average1 img =
  filtr_simpl_img img (average3()) 3

(* average5 filter *)
let average2 img =
   filtr_simpl_img img (average5()) 5
  
                    

(* ------------------------------------ ------------------------------------- *)

(* END *)
