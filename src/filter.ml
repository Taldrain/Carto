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
  mat.(0).(0) <-  1;
  mat.(1).(0) <-  0;
  mat.(2).(0) <-(-1);
  mat.(0).(1) <-  2;
  mat.(1).(1) <-  0;
  mat.(2).(1) <-(-2);
  mat.(0).(2) <-  1;
  mat.(1).(2) <-  0;
  mat.(2).(2) <-(-1);
  mat

let sobel2() =
  let mat = Array.make_matrix 3 3 0 in
  mat.(0).(0) <-  1;
  mat.(1).(0) <-  2;
  mat.(2).(0) <-  1;
  mat.(0).(1) <-  0;
  mat.(1).(1) <-  0;
  mat.(2).(1) <-  0;
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


let gauss3() (* var *) =
  let mat = Array.make_matrix 3 3 0 in
(*  let grad x y = int_of_float(( 1. /. ( 2. *. 3.1416 *. var ** 2.))
                  *. exp ( -. ( (float)(x) ** 2. +. (float)(y) ** 2. ) /. 2. *.
                  ( var ** 2.) )) in  *)
  mat.(0).(0) <- 1;
  mat.(1).(0) <- 2;
  mat.(2).(0) <- 1;
  mat.(0).(1) <- 2;
  mat.(1).(1) <- 4;
  mat.(2).(1) <- 2;
  mat.(0).(2) <- 1;
  mat.(1).(2) <- 2;
  mat.(2).(2) <- 1;
  mat



let gauss5() =
  let mat = Array.make_matrix 5 5 0 in
  mat.(0).(0) <- 2;
  mat.(1).(0) <- 4;
  mat.(2).(0) <- 5;
  mat.(3).(0) <- 4;
  mat.(4).(0) <- 2;
  mat.(0).(1) <- 4;
  mat.(1).(1) <- 9;
  mat.(2).(1) <-12;
  mat.(3).(1) <- 9;
  mat.(4).(1) <- 4;
  mat.(0).(2) <- 5;
  mat.(1).(2) <-12;
  mat.(2).(2) <-15;
  mat.(3).(2) <-12;
  mat.(4).(2) <- 5;
  mat.(0).(3) <- 4;
  mat.(1).(3) <- 9;
  mat.(2).(3) <-12;
  mat.(3).(3) <- 9;
  mat.(4).(3) <- 4;
  mat.(0).(4) <- 2;
  mat.(1).(4) <- 4;
  mat.(2).(4) <- 5;
  mat.(3).(4) <- 4;
  mat.(4).(4) <- 2;
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
  for y = 0 to 4 do
    for x = 0 to 4 do
      mat.(x).(y) <- 1;
    done;
  done;
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
let mpf3 img x y filter color coef =
  try
  (filter.(0).(0) * (atb_p img (x-1) (y-1) color ) +
   filter.(1).(0) * (atb_p img (x  ) (y-1) color ) +
   filter.(2).(0) * (atb_p img (x+1) (y-1) color ) +
   filter.(0).(1) * (atb_p img (x-1) (y  ) color ) +
   filter.(1).(1) * (atb_p img (x  ) (y  ) color ) +
   filter.(2).(1) * (atb_p img (x+1) (y  ) color ) +
   filter.(0).(2) * (atb_p img (x-1) (y+1) color ) +
   filter.(1).(2) * (atb_p img (x  ) (y+1) color ) +
   filter.(2).(2) * (atb_p img (x+1) (y+1) color )) / coef
  with _ -> 0

(* addition of pixel with all neighbours filtered (3x3) to make pixel filtered*)
let multi3_pix_filter img x y filter coef =
  let pixel = (mpf3 img x y filter "r" coef ,mpf3 img x y filter "g" coef
                                      ,mpf3 img x y filter "b" coef) in
    normalize pixel

(* function needed for the next function 5x5 *)
let mpf5 img x y filter color coef =
 try
  let int_f = ref 0 in
    for j = 0 to 4 do
       for i = 0 to 4 do
        int_f := !int_f + filter.(i).(j) * (atb_p img (x-2+i) (y-2+j) color );
       done;
    done;
  !int_f / coef
 with _ -> 0


(* addition of pixel with all neighbours filtered (5x5) to make pixel filtered*)
let multi5_pix_filter img x y filter coef =
  let pixel = (mpf5 img x y filter "r" coef ,mpf5 img x y filter "g" coef
                                      , mpf5 img x y filter "b" coef) in
    normalize pixel


(* multiplication of filter and the matrix (3x3) *)
let multi_mat_filter filter img dim coef =
  let (w,h) = ((Sdlvideo.surface_info img).Sdlvideo.w,
              (Sdlvideo.surface_info img).Sdlvideo.h) in
  let mat_out = Array.make_matrix w h (0,0,0) in
    for x = 0 to w - 1 do
     for y = 0 to h - 1 do
       if dim = 3 then
         mat_out.(x).(y) <- multi3_pix_filter img x y filter coef
       else  mat_out.(x).(y) <- multi5_pix_filter img x y filter coef;
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
let filtr_simpl_img img filter dim coef =
  let mat_f = multi_mat_filter filter img dim coef in
    mat_to_img mat_f img

(* image to image double filtered *)
let filtr_doubl_img img filter1 filter2 dim coef =
  let mat1 = multi_mat_filter filter1 img dim coef in
  let mat2 = multi_mat_filter filter2 img dim coef in
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

(* ----------------------- Functions for sobel filter ----------------------- *)

(* Sobel filter   image -> image   *)
let sobel_filter img =
  mat_to_img (filtr_doubl_img img (sobel1()) (sobel2()) 3 9) img

let sobel_filter2 img =
  mat_to_img (filtr_doubl_img img (sobelv2_1()) (sobelv2_2()) 3 9) img



(* ------------------------------------ ------------------------------------- *)

(* ------------------------- Function for median filter --------------------- *)

(* give the red component of pixel *)
let red mat x y = 
  let (r,_,_) = mat.(x).(y) in
    r

(* mdian filter, it has a different behaviour *)
let median_filtr img dim =
  let mat = img_to_mat img in
  let mat_f = Array.make_matrix (Array.length mat)
              (Array.length mat.(0)) (0,0,0) in
    for x = 0 to Array.length mat - 1 do
      for y = 0 to Array.length mat.(0) - 1 do
        try
          let li = ref [] in
          if dim = 3 then
            li := [red mat (x-1) (y-1); red mat (x) (y-1); red mat (x+1) (y-1);
            red mat (x-1) (y); red mat (x) (y); red mat (x+1) (y);
            red mat (x-1) (y+1); red mat (x) (y+1); red mat (x+1) (y+1)];
          else  
            li := [red mat (x+1) (y+1); red mat (x+1) (y+1); red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);red mat (x+1) (y+1);
          let li_f = List.fast_sort (fun x y -> compare x y) (!li) in
          let g = List.nth li_f 4 in
          mat_f.(x).(y) <- (g,g,g);
        with Invalid_argument "index out of bounds" ->
               mat_f.(x).(y) <- mat.(x).(y);
      done;
    done;
    mat_to_img mat_f img



(* ------------------------------------ ------------------------------------- *)

(* ---------------------------- Filter functions ---------------------------- *)

(* complete sobel filter with colored edges*)
let sobel_filter_f_color img =
  mat_to_img (mat1_mat2 (filtr_doubl_img img (sobel1()) (sobel2()) 3 9)
                  (filtr_doubl_img img (sobelv2_1()) (sobelv2_2()) 3 9) img) img

(* sobel filter with white edges *)
let sobel_filter_f img =
  mat_edge_to_white (mat1_mat2 (filtr_doubl_img img (sobel1()) (sobel2()) 3 9)
                  (filtr_doubl_img img (sobelv2_1()) (sobelv2_2()) 3 9) img) img

(* average3 filter *)
let average1 img =
  filtr_simpl_img img (average3()) 3 9

(* average5 filter *)
let average2 img =
   filtr_simpl_img img (average5()) 5 25

(* gauss3 filter *)
let gauss3_filter img variance =
  filtr_simpl_img img (gauss3()) 3 16

(* median filter, with 5x5 matrix *)
let median_filtr5 img =
  median_filtr img 5

(* median filter, with 3x3 matrix *)
let median_filtr3 img =
  median_filtr img 3



(* ------------------------------------ ------------------------------------- *)

(* END *)
