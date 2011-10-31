(* POST TREATMENT *)
(* variables globales *)

(* ((int*int*int)*int)list *)
let li_ord = ref []

(*fonctions generales *)
let altitude pix =
  let rec alt pi li = match li with
  | [] -> 0
  | e::_ when e.Refe.rgb = pi -> e.Refe.alt
  | e::l -> alt pi l in
  alt pix (Refe.get_list_alt())

let get_alt() =
  begin
    for y = 0 to (Refe.get_h())/( Refe.get_step()) do
      for x = 0 to (Refe.get_w())/(Refe.get_step()) do
	   begin
		 let ((d,e), b) = (Refe.get_matrice_rgb()).(x).(y) in
		 let alti = altitude b in 
	     Array.set (Refe.get_matrice_ret()).(x) (y) ((d, e, alti),b)
       end 
	  done;
    done;
  end

let get_f x y = (Refe.get_matrice_ret()).(x).(y)

(* ((int*int*int)*(int*int*int))array array -> 
   (int*int*int)list *) 
let mat_to_lixyz() =
  let rec mtlix x y = match (x,y) with
    | (x,y) when y > ((Refe.get_w())/(Refe.get_step()))
	-> []
    | (x,y) when x > ((Refe.get_h())/(Refe.get_step()))
	-> mtlix 1 (y+1)
    | (x,y) -> get_f x y::(mtlix (x+1) y)
  in mtlix 0 0

(* creation de liste ordonnees necessaire a la 3D
   -> ((int*int*int)*(int*int*int))list  *)
let mat_to_li() =
  let rec mtli x y = match (x,y) with
    | (x,y) when y > ((Refe.get_w())/(Refe.get_step()))
	-> []
    | (x,y) when x > ((Refe.get_h())/(Refe.get_step()))
	-> mtli 1 (y+1)
    | (x,y) -> (get_f (x-1) y)::(get_f (x-1) (y-1))
	::(get_f x (y-1))::(get_f (x-1) y)::(get_f x y)
	::(get_f x (y-1))::(mtli (x+1) y)
  in mtli 1 1

let rec i_2_f = function
    [] -> []
  | e::l -> let ((x,y,z),(r,g,b)) = e in ((float(x),float(y),float(z)),
					(float(r),float(g),float(b)))
					::i_2_f l


(*FONCTIONS NECESSAIRES AU .OBJ *)

(* (int*int*int)list -> ((int*int*int)*int)list avec le dernier int le numero de
l'element *)
let list_tolist2 li = 
  let rec li_tli2 l incr = match l with
    | [] -> []
    | e::l -> let (a,b) = e in (a,incr)::(li_tli2 l (incr+1)) 
  in li_tli2 li 1

(*  (int*int*int) -> (int*int*int)*int)list  -> int *)
let rec ident_elt elt li = match li with 
	| [] -> (-1)
	| e::li -> let (a,b) = e in if a = elt then b else (ident_elt elt li) 


(* (int*int*int)*(int*int*int)list -> ((int*int*int)*int)list -> int list *)    
let rec ident_li li li2 = match li with
	| [] -> []
	| e::li -> let ((a,b,c),d) = e in 
        (ident_elt (int_of_float(a),int_of_float(b),
            int_of_float(c)) li2)::(ident_li li li2)


(*fonction qui ecrit la ligne correspondante d'un triplet dans l'obj*)
let str_of_tri tr = let (a,b,c) = tr in
  "v "^string_of_float(float_of_int(a))^" "^
    string_of_float(float_of_int(b))^" "^string_of_float(float_of_int(c))^"\n"


let write_obj() =
  begin
    let f = open_out "supermap.obj" in
    let lis = (ident_li (Refe.get_list_3d()) (!li_ord)) in
      output_string f "g topoteam\n\n";
      let rec wri_obj li = match li with
	| [] -> ()
	| (a,_)::li ->
	    begin
	      output_string f (str_of_tri a);
	      wri_obj li;
	    end
      in wri_obj (Refe.get_list_xyz());
      (* lis = int list *)
       output_string f "\n";
		 let rec wri_obj2 l incr = match (l,incr) with    
		 	| ([],incr) -> ()
			| (e::l, incr) when (incr mod 3 = 1) 
                -> begin 
                    output_string f ("f "^string_of_int(e));
                    wri_obj2 l (incr+1);
                   end
            | (e::l, incr) when  (incr mod 3 = 0) 
                -> begin  
                    output_string f (" "^string_of_int(e)^"\n");
                    wri_obj2 l (incr+1);
                   end 
            | (e::l, incr) 
                -> begin 
                    output_string f (" "^string_of_int(e));
                    wri_obj2 l (incr+1);
                   end   
         in wri_obj2 lis 1; 
	close_out f;
  end


(*fonction main du post_treatment *)
let post_treat() =
  begin
    get_alt();
    Refe.list_3d := i_2_f (mat_to_li());
    Refe.list_xyz := (mat_to_lixyz());
    li_ord := list_tolist2 (Refe.get_list_xyz());
    (*write_obj();*)
  end

