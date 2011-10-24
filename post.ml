(* POST TREATMENT *)

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
	let ((d,e), b) = (Refe.get_matrice_rgb()).(x).(y) in
	let alti = altitude b in
	print_endline (string_of_int alti);
	  Array.set (Refe.get_matrice_ret()).(x) (y) ((d, e, alti),b)
      done;
    done;
  end


let get_f x y = (Refe.get_matrice_ret()).(x).(y)

let mat_to_lixyz() =
  let rec mtlix x y = match (x,y) with
    | (x,y) when y > ((Refe.get_w())/(Refe.get_step()))
	-> [] 
    | (x,y) when x > ((Refe.get_h())/(Refe.get_step()))
	-> mtlix 1 (y+1)
    | (x,y) -> get_f x y::(mtlix (x+1) y)
  in mtlix 0 0

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


(* creation du .OBJ *)

(*fonction qui ecrit la ligne correspondante d'un triplet dans l'obj*)
let str_of_tri tr = let (a,b,c) = tr in
  "v "^string_of_int(a)^" "^
    string_of_int(b)^" "^string_of_int(c)^"\n"



let write_obj() =
  begin
    let f = open_out "supermap.obj" in
      output_string f "g topoteam\n\n";
      let rec wri_obj li = match li with
	| [] -> ()
	| (a,_)::li ->
	    begin
	      output_string f (str_of_tri a);
	      wri_obj li;
	    end
      in wri_obj (Refe.get_list_xyz());
	close_out f;
  end


(*fonction main du post_treatment *)
let post_treat() =
  begin
    get_alt();
    Refe.list_3d := i_2_f (mat_to_li());
    Refe.list_xyz := (mat_to_lixyz());
    write_obj(); 
  end

