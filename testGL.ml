(* Matrice3 - (x, y, z)
 * avec x, y et z des floats
 * gauche <= 0<x>0 => droite
 * de meme pour y avec bas et haut
 * z represente la profondeur
 * si z > 0 z est devant l'ecran donc non visible
 * si z < 0 z est derriere l'ecran donc "plus petit"
 *)

(* Vecteur3
 * placement d'un "point"
 * apres cette operation la "matrice" reprend sa precedent place initiale
 * meme regle pour les parametres que pour les matrices
 *)

let width = 640
let height = 480

let setup width height =
  (* permettre le degrade de couleur *)
  GlDraw.shade_model `smooth;
  (* couleur de fond *)
  GlClear.color (0.0, 0.0, 0.0);
  (* profondeur *)
  GlClear.depth 1.0;
  GlClear.clear [`color; `depth];
  (* ? *)
  Gl.enable `depth_test;
  GlFunc.depth_func `lequal;
  GlMisc.hint `perspective_correction `nicest

let scene_gl () =
  (* precaution *)
  GlClear.clear [`color; `depth];
  (* creer une matrice *)
  GlMat.load_identity ();
  (* changement "d'origine" de la matrice *)
  GlMat.translate3 (0.0, 0.0, -5.0);
  (* couleur courante  (<= 1, avec 1 = blanc) *)
  GlDraw.color (1.0, 0.0, 1.0);
  (* test d'un carre *)
  (* tout les tags ne sont pas permis, nous utiliserons `triangles *)
  GlDraw.begins `quads;
  GlDraw.vertex3 (-1.0, 1.0, -10.0);
  GlDraw.color (1.0, 1.0, 1.0);
  GlDraw.vertex3 (1.0, 1.0, 0.0);
  GlDraw.color (0.0, 0.0, 1.0);
  GlDraw.vertex3 (1.0, -1.0, 0.0);
  GlDraw.color (1.0, 1.0, 0.0);
  GlDraw.vertex3 (-1.0, -1.0, 0.0);
  GlDraw.vertex3 (-1.0, 1.0, 1.0);
  GlDraw.ends ();
  (* ? *)
  Glut.swapBuffers ()


(* redimensionner et lancement du programme *)
let reshape ~w ~h =
  let ratio = (float_of_int w) /. (float_of_int h) in
    (* limite d'affichage *)
    GlDraw.viewport 0 0 w h;
    (* mode projection ? *)
    GlMat.mode `projection;
    (* chargement de la matrice identite *)
    GlMat.load_identity ();
    (* ? *)
    GluMat.perspective 45.0 ratio (0.1, 100.0);
    (* changement de mode ? *)
    GlMat.mode `modelview;
    GlMat.load_identity ()

let keyboard_event ~key ~x ~y = match key with
    (* ESCAPE *)
    27 -> exit 0
  | _ -> ()


let main () =
    ignore (Glut.init Sys.argv);
    (* creation du mode d'affichage *)
    Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true ();
    (* Init de la fenetre, a remplacer par une fenetre gtk *)
    Glut.initWindowSize width height;
    (* nom de la fenetre - ne sera plus necessaire *)
    ignore (Glut.createWindow "hello");
    (* creation de la scene *)
    Glut.displayFunc scene_gl;
    (* gestion du clavier *)
    Glut.keyboardFunc keyboard_event;
    (* reshape *)
    Glut.reshapeFunc reshape;
    (* init *)
    setup width height;
    Glut.mainLoop ()

let _ = main ()
