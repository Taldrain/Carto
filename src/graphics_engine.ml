(* Graphics engine *)

let rx = ref (-90.)
let ry = ref 0.
let rz = ref 0.
let dtx() = (float (-(Refe.get_w()/(2*Refe.get_step()))))
let tx = ref 0.
let dty() = (float (-(Refe.get_w()/(5*Refe.get_step()))))
let ty = ref 0.
let dtz() = (float (-((Refe.get_h())/Refe.get_step())))
let tz = ref 0.
let line = ref true
let lr = ref 0.
let lg = ref 0.
let lb = ref 0.
let lx = ref 0.
let ly = ref 0.
let lz = ref 0.


let init () =
  ignore(Glut.init Sys.argv);
  (* creation du mode d'affichage *)
  (*Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true ();*)
  (* Init de la fenetre, a remplacer par une fenetre gtk *)
  (*Glut.initWindowSize width height;*)
  (* permettre le degrade de couleur *)
  GlDraw.shade_model `smooth;
  (* couleur de fond *)
  GlClear.color (0.0, 0.0, 0.0);
  (* profondeur *)
  GlClear.depth 1.;
  (* precaution *)
  GlClear.clear [`color; `depth];
  Gl.enable `depth_test;
  GlFunc.depth_func `lequal;
  GlMisc.hint `perspective_correction `nicest

let findcolor = function
  | (a,b,c) -> lr:= a; lg:= b; lb:= c

let rec create_tri = function
    [] -> ()
  | e::l -> (*findcolor (snd e);*)
            GlDraw.color (snd e);
            GlDraw.vertex3 (fst e);
            create_tri l


(* affichage de la scene *)
let display () =
  (* precaution *)
  GlClear.clear [`color; `depth];
  (* creer une matrice *)
  GlMat.load_identity ();
  (* changement "d'origine" de la matrice *)
  GlMat.translate3 (!tx, !ty, !tz);
  (* translation *)
  GlMat.rotate3 !rx (2.0, 0.0, 0.0);
  GlMat.rotate3 !ry (0.0, 2.0, 0.0);
  GlMat.rotate3 !rz (0.0, 0.0, 2.0);
  (* Modifier le mode d'affichage *)
  if !line then
    (GlDraw.polygon_mode `front `line;
    GlDraw.polygon_mode `back `line)
  else
    (GlDraw.polygon_mode `front `fill;
    GlDraw.polygon_mode `back `fill);
  GlDraw.line_width 1.0;
  GlDraw.begins `triangles;
  create_tri (Refe.get_list_3d());
  GlDraw.ends ();
  tx := !tx -. 0.1;
  Gl.flush ()



(* redimensionner et lancement du programme *)
let reshape ~width:w ~height:h =
    (* limite d'affichage *)
    GlDraw.viewport ~x:0 ~y:0 ~w ~h;
    (* mode projection ? *)
    GlMat.mode `projection;
    (* chargement de la matrice identite *)
    GlMat.load_identity ();
    GluMat.perspective 45. 1. (1., 500.0);
    (* changement de mode ? *)
    GlMat.mode `modelview;
    GlMat.load_identity ()


