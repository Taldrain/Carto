(* Graphics engine *)

let width = 640
let height = 480
let rx = ref 0.0
let ry = ref 0.0
let rz = ref 0.0
let line = ref true
let liste = ref (((0.4, 0.6, 0.3), (20., 4., 3.))::
                 ((1.0, 1.0, 1.0), (1.0, 1.0, 0.0))::
                 ((0.0, 0.0, 1.0), (1.0, -1.0, 0.0))::
                 ((1.0, 1.0, 0.0), (20., 4., 3.))::
                 ((0.0, 1.0, 1.0), (1.0, 1.0, 0.0))::
                 ((1.0, 1.0, 1.0), (9.0, 4.0, 6.0))::
                 ((0.0, 1.0, 0.0), (20., 4., 3.))::
                 ((0., 1., 0.), (1.0, -1.0, 0.0))::
                 ((1., 0., 1.),(9., 4., 6.))::[])



let setup () =
  (* permettre le degrade de couleur *)
  GlDraw.shade_model `smooth;
  (* couleur de fond *)
  GlClear.color (0.0, 0.0, 0.0);
  (* profondeur *)
  GlClear.depth 5.;
  GlClear.clear [`color; `depth];
  Gl.enable `depth_test;
  GlFunc.depth_func `lequal;
  GlMisc.hint `perspective_correction `nicest


let rec create_tri = function
    [] -> ()
  | e::l -> GlDraw.color (fst e);
            GlDraw.vertex3 (snd e);
            create_tri l


(* affichage de la scene *)
let scene_gl () =
  (* precaution *)
  GlClear.clear [`color; `depth];
  (* creer une matrice *)
  GlMat.load_identity ();
  (* changement "d'origine" de la matrice *)
  GlMat.translate3 (0.0, 0.0, -10.0);
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
  GlDraw.line_width 3.0;
  GlDraw.begins `triangles;
  create_tri !liste;
  GlDraw.ends ();
  Glut.swapBuffers ()(*;
  rx := !rx +. 5.;
  ry := !ry +. 5.;
  rz := !rz +. 5.*)


(* redimensionner et lancement du programme *)
let reshape ~w ~h =
  let ratio = (float_of_int w) /. (float_of_int h) in
    (* limite d'affichage *)
    GlDraw.viewport 0 0 w h;
    (* mode projection ? *)
    GlMat.mode `projection;
    (* chargement de la matrice identite *)
    GlMat.load_identity ();
    GluMat.perspective 45.0 ratio (0.1, 100.0);
    (* changement de mode ? *)
    GlMat.mode `modelview;
    GlMat.load_identity ()
    

(* fonction xor *)
let xor a b =
  if a = true then
    (if b then false else true)
  else
    (if b then true else false)

(* gestion des evenements du clavier *)
let keyboard_event ~key ~x ~y = match key with
    (* ESCAPE *)
    27 -> exit 0
  (* touche "q" *)
  | 113 -> rx := !rx +. 5.0
  (* touche "w" *)
  | 119 -> rx := !rx -. 5.0
  (* touche "a" *)
  | 97 -> ry := !ry +. 5.0
  (* touche "s" *)
  | 115 -> ry := !ry -. 5.0
  (* touche "z" *)
  | 122 -> rz := !rz +. 5.0
  (* touche "x" *)
  | 120 -> rz := !rz -. 5.0
  | 108 -> line := (xor !line true)
  | _ -> ()


(* fonction d'idle *)
let idle () =
  scene_gl ()


let main () =
    ignore (Glut.init Sys.argv);
    (* creation du mode d'affichage *)
    Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true ();
    (* Init de la fenetre, a remplacer par une fenetre gtk *)
    Glut.initWindowSize width height;
    ignore (Glut.createWindow "hello");
    (* creation de la scene *)
    Glut.displayFunc scene_gl;
    (* gestion du clavier *)
    Glut.keyboardFunc keyboard_event;
    Glut.reshapeFunc reshape;
    Glut.idleFunc(Some idle);
    setup ();
    Glut.mainLoop ()

let _ = main ()
