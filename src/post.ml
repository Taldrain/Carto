(* POST TREATMENT *)
(* variables globales *)

(* ((int*int*int)*int)list *)
let li_ord = ref []

(*fonctions generales *)
let altitude pix =
  let rec alt pi li = match li with
  | [] -> failwith "Pas d'altitude";
  | e::_ when e.Refe.orig_color = pi -> (e.Refe.alt, e.Refe.rgb)
  | e::l -> alt pi l in
  alt pix (Refe.get_list_alt())

let get_alt() =
  begin
    for y = 0 to (Refe.get_h())/( Refe.get_step()) do
      for x = 0 to (Refe.get_w())/(Refe.get_step()) do
	   begin
		 let ((d,e), b) = (Refe.get_matrice_rgb()).(x).(y) in
		 let alti = altitude b in
		 match alti with
		 	| (al, co) ->
	     Array.set (Refe.get_matrice_ret()).(x) (y) ((d, e, al),co)
       end
	  done;
    done;
  end





(* [[DEBUT]] SUITE DE FONCTIONS POUR LISSAGE DES ALTITUDES *)
(* 0 1 2
   7 x 3
   6 5 4 *)

let red_pill matrix x y =
	if ((x < (Array.length matrix)) &&
	    (x >= 0) &&
	    (y < (Array.length matrix.(0))) &&
	    (y >= 0)) then true
	else false

let get_h = function
	| (a, b, h),e -> h

let set_newH newh = function
	| ((a, b, _), (r, g, bb)) -> ((a, b, newh), (r, g, bb))

let liss () =
	let matrix = Refe.get_matrice_ret () in
	let new_matrix = Refe.get_matrice_fin () in
    for y = 0 to (Refe.get_h())/( Refe.get_step()) do
      for x = 0 to (Refe.get_w())/(Refe.get_step()) do
	   begin
	   		let sum = ref (get_h matrix.(x).(y)) in
			let nb = ref 1 in
			begin
				if (red_pill matrix (x-1) (y-1)) then
					sum := !sum + (get_h matrix.(x-1).(y-1));
					nb := !nb + 1;
			end;
			begin
				if (red_pill matrix x (y-1)) then
					sum := !sum + (get_h matrix.(x).(y-1));
					nb := !nb + 1;
			end;
			begin
				if (red_pill matrix (x+1) (y-1)) then
					sum := !sum + (get_h matrix.(x+1).(y-1));
					nb := !nb + 1;
			end;
			begin
				if (red_pill matrix (x+1) y) then
					sum := !sum + (get_h matrix.(x+1).(y));
					nb := !nb + 1;
			end;
			begin
				if (red_pill matrix (x+1) (y+1)) then
					sum := !sum + (get_h matrix.(x+1).(y+1));
					nb := !nb + 1;
			end;
			begin
				if (red_pill matrix x (y+1)) then
					sum := !sum + (get_h matrix.(x).(y+1));
					nb := !nb + 1;
			end;
			begin
				if (red_pill matrix (x-1) (y-1)) then
					sum := !sum + (get_h matrix.(x-1).(y-1));
					nb := !nb + 1;
			end;
			begin
				if (red_pill matrix (x-1) y) then
					sum := !sum + (get_h matrix.(x-1).(y));
					nb := !nb + 1;
			end;
		Array.set new_matrix.(x) (y) (set_newH ((!sum)/(!nb)) matrix.(x).(y));
       end;
	  done;
    done
(* [[FIN]] SUITE DE FONCTIONS POUR LISSAGE DES ALTITUDES *)

let get_f x y = (Refe.get_matrice_fin()).(x).(y)

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

(* (int*int*int)list -> ((int*int*int)*int)list avec le dernier int le numero
de l'element *)
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








(* --------------  OPTIMISATION DE LA TRIANGULATION ------------------------- *)

(* variables globales necessaires a la triangularisation *)
let x0 = ref 0
let y0 = ref 0
let z0 = ref 0
let xx = ref 0
let yx = ref 0
let xy = ref 0
let yy = ref 0

(* liste des point a l interieur d un triangle *)
let list_in_tri_coords = ref ( [] : (int*int) list )


(* stocker les couples (x,y) appartenent a un triangle,
	dans list_in_tri_coords, apres test_x *)
let stock_in_tri_coordsX x y =
		if ( ( (x-(!x0)) > 1 ) && ( (y-(!y0)) > 2) ) then
		for i=1 to (y-(!y0)) do
				list_in_tri_coords:=((x-1),((!y0)+i))::(!list_in_tri_coords)
		done

(* stocker les couples (x,y) appartenant a un triangle,
	dans list_in_tri_coords, apres test_y *)
let stock_in_tri_coordsY x y =
	if ( ( (x-(!x0)) > 2 ) && ( (y-(!y0)) > 1 )) then
		for i=1 to (x-(!x0)) do
				list_in_tri_coords:=(((!x0)+i),(y-1))::(!list_in_tri_coords)
		done


(* tester si (x,y) appartient deja a un triangle - 3rd version *)
(* principe: tester si (x,y) appartient ou non a une liste de couplet,
	cette liste est generer dans test_x et test_y *)
let rec test_membership_3 x y = function
	| [] -> false
	| (a,b)::l -> if (x=a) && (y=b) then true
					else test_membership_3 x y l


(* test d altitude sur la largeur *)
let test_x x y =
	if (test_membership_3 x y (!list_in_tri_coords)) then false
	else let i = ref 0 in
		let get_z ((_,_,c),_) = c in 
		let zi e f = get_z (get_f e f) in
			while ((!i) <  ((y-(!y0))+1)) 
					&& ( (zi x ((!y0)+(!i))) = (!z0)) do
				i := !i + 1
			done;
		if (zi x ((!y0)+(!i))) = (!z0) then
				begin
					stock_in_tri_coordsX x y;
					true
				end
			else false

	
(* test d altitude su la hauteur *)
let test_y x y =
	if (test_membership_3 x y (!list_in_tri_coords)) then false
	else let j = ref 0 in
		let get_z ((_,_,c),_) = c in
		let zj e f = get_z (get_f e f) in
			while ((!j) < ((x-(!x0))+1)) && ( (zj ((!x0)+(!j)) y) = (!z0)) do
				j := !j + 1
			done;
		if (zj ((!x0)+(!j)) y) = (!z0) then
				begin
					stock_in_tri_coordsY x y;
					true
				end
			else false


(* creer la liste de coordonnees des grands triangles *)
let stock_tri3D x0 y0 x1 y1 x2 y2 =
	let f_of_i ((a,b,c),(d,e,f)) =
		(((float_of_int a),(float_of_int b),(float_of_int c)),
		((float_of_int d),(float_of_int e),(float_of_int f))) in
			Refe.list_tri3D :=
			(f_of_i (get_f x1 y1))
			::(f_of_i (get_f x1 y0))
			::(f_of_i (get_f x0 y0))
			::(f_of_i (get_f x2 y2))
			::(f_of_i (get_f x0 y2))
			::(f_of_i (get_f x0 y0))
			::(Refe.get_list_tri3D())

(* chercher le plus grand triangle possible en partant de (x0,y0) *)
let new_triangles x y =
	x0 := x;
	y0 := y;
	let get_z ((_,_,c),_) = c in
	z0 := get_z (get_f (!x0) (!y0));
		let rec tri_max x y =
			let testx = test_x x y in
			let testy = test_y x y in
			begin
				if (testx && testy) then
					begin
						match (x,y) with
						| (x,y) when 
							y > ((Refe.get_w())/(Refe.get_step())-1) -> 
									begin
										xx := (x-1);
										yx := (y-1);
										xy := (x-1);
										yy := (y-1)
									end;
						| (x,y) when 
							x > ((Refe.get_h())/(Refe.get_step())) -> 
									begin
										xx := (x-1);
										yx := (y-1);
										xy := (x-1);
										yy := (y-1)
									end;
						| (x,y) ->  tri_max (x+1) (y+1);
					end;
				if ( (not testx) || (not testy) ) then
					begin
						xx := (x-1);
						yx := (y+1);
						xy := (x+0);
						yy := (y+1)
					end;
			end;
(* maintenant que on a les coordonnées des sommet des triangles max,
il faut les stockés *)
	stock_tri3D !x0 !y0 !xx !yx !xy !yy
		in tri_max !x0 !y0



(* fonction principale de la triangulation,
qui consiste a tester (x,y) , s'il font déja parti d'un triangle, x->x+1,
si x depasse la largeur, x->0 et y->y+1, si y depasse la hauteur, sortir,
sinon new_triangles x y *)
let triangulation () =
(* Remise a zero des valeurs *)
	x0 := 0;
	y0 := 0;
	z0 := 0;
	xx := 0;
	yx := 0;
	xy := 0;
	yy := 0;
	list_in_tri_coords := [];

	let rec tri x y = match (x,y) with
		| (x,y) when y > ((Refe.get_w())/(Refe.get_step())-1) -> ()
		| (x,y) when x > ((Refe.get_h())/(Refe.get_step())) -> tri 1 (y+1)
		| (x,y) when (test_membership_3 x y (!list_in_tri_coords)) 
			-> tri (x+1) y
		| (x,y) -> begin
						new_triangles x y;
						tri (x+1) y
				   end;
	in tri 1 1


(* DEBUG AFFICHAGE List_tri3D *)
let print_liste l =
	print_endline "-----------------------------------------";
	let rec print_list liste i =
		match liste with
		| [] -> ()
		| ((x,y,z),(r,g,b))::l -> 
									print_string "((";
									print_float x;
									print_string ",";
									print_float y;
									print_string ",";
									print_float z;
									print_string "),(";
									print_float r;
									print_string ",";
									print_float g;
									print_string ",";
									print_float b;
									print_endline "))";
									if ( i < 3) then
										print_list l (i+1)
									else
										(
										print_endline "***********************";
										print_list l 1
										)
		in print_list l 1;
	print_endline "-----------------------------------------"




(*fonction main du post_treatment *)
let post_treat() =
  begin
    get_alt();
	liss ();
    Refe.list_3d := i_2_f (mat_to_li());
    Refe.list_xyz := (mat_to_lixyz());
    li_ord := list_tolist2 (Refe.get_list_xyz());
	triangulation ();
	(*print_liste (Refe.get_list_tri3D ());*)
    if (Refe.is_save_obj ()) then
        write_obj();
  end


