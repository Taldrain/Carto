(* Graphics engine *)

(* mouse var *)
let xold = ref 0
let yold = ref 0
let bl_down = ref false

let anglex = ref 0
let angley = ref 0

let pas = ref 2.

type mode = Fill | Line | Point
let mode_ = ref Line

let g_mode = function
  | Fill -> `fill
  | Line -> `line
  | Point -> `point

let light_b = ref false

let rx = ref (-40.)
let ry = ref 0.
let rz = ref 0.
let dtx() = (float (-(Refe.get_w()/(2*Refe.get_step()))))
let tx = ref 0.
let dty() = (float (-(Refe.get_w()/(5*Refe.get_step()))))
let ty = ref 0.
let dtz() = (float (-((Refe.get_h())/Refe.get_step())))
let tz = ref 0.
let lr = ref 0.
let lg = ref 0.
let lb = ref 0.
let lx = ref 0.
let ly = ref 0.
let lz = ref 0.


let init () =
  (* init des valeurs de camera *)
  tx := dtx();
  ty := dty();
  tz := dtz();
  ignore (Glut.init Sys.argv);
  (* creation du mode d'affichage *)
  Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true ();
  (* Init de la fenetre, a remplacer par une fenetre gtk *)
  Glut.initWindowSize ~w:800 ~h:600;
  ignore (Glut.createWindow "hello");
  (* permettre le degrade de couleur *)
  GlDraw.shade_model `smooth;
  (* couleur de fond *)
  GlClear.color (0.0, 0.0, 0.0);
  (* profondeur *)
  GlClear.depth 1.;
  (* precaution *)
  GlClear.clear [`color; `depth];
  List.iter Gl.enable [`depth_test; `lighting; `light0];
  (*GlFunc.depth_func `lequal;*)
  GlFunc.depth_func `less;
  GlMisc.hint `perspective_correction `nicest


(*let init_light () =
  let light_ambient = !lr, !lg, !lb, 1.
  and light_diffuse = 1., 0., 0., 1.(*!lr*.2., !lg*.2., !lb*.2., 1.*)
  and light_specular = 0., 1., 0., 1.(*!lr*.4., !lg*.4., !lb*.4., 1.*)
  and light_position = !lx, !ly, !lz, 1.
  in
  List.iter (GlLight.light ~num:0)
    [ `ambient light_ambient;
      `diffuse light_diffuse;
      `specular light_specular;
      `position light_position ]
  (*GlDraw.shade_model `smooth*)*)


(*let findcolor = function
  | (a,b,c) -> lr:= a; lg:= b; lb:= c*)


(*let distance_lp (x2,y2,z2) =
  sqrt ((x2-.(!lx))*.(x2-.(!lx))+.(y2-.(!ly))*.(y2-.(!ly))+.(z2-.(!lz))*.(z2-.(!lz)))

let attenuation (x2,y2,z2) =
  max(0.,1. -. ((distance_lp (x2,y2,z2))/.z2))

let light (x2,y2,z2) =
  let a = attenuation (x2,y2,z2) in
    (x2*.a, y2*.a, z2*.a)
 *) 


let rec create_tri = function
    [] -> ()
  | e::l -> (*GlDraw.color ((attenuation (snd e))*(snd e));
             *)
            GlDraw.color (snd e);
            GlDraw.vertex3 (fst e);
            create_tri l


(* affichage de la scene - display *)
let scene_gl () =
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
  GlDraw.polygon_mode `both (g_mode !mode_);
  GlDraw.line_width 1.0;
  GlDraw.begins `triangles;
  create_tri (Refe.get_list_3d());
  GlDraw.ends ();
  if !light_b then
    Gl.enable `lighting
  else
    Gl.disable `lighting;
  Glut.swapBuffers ()


(* redimensionner et lancement du programme *)
let reshape ~w ~h =
  let ratio = (float_of_int w) /. (float_of_int h) in
    (* limite d'affichage *)
    GlDraw.viewport 0 0 w h;
    (* mode projection ? *)
    (*GlMat.mode `projection;*)
    (* chargement de la matrice identite *)
    (*GlMat.load_identity ();*)
    (* changement de mode ? *)
    GlMat.mode `projection;
    GlMat.load_identity ();
    GlMat.rotate ~angle:(float(- !anglex)) ~x:0. ~y:0. ~z:1. ();
    GlMat.rotate ~angle:(float(- !angley)) ~x:1. ~y:0. ~z:0. ();
    GluMat.perspective ~fovy:45. ~aspect:ratio ~z:(0.1,500.);
    GlMat.mode `modelview;
    GlMat.load_identity ();
    Gl.flush()


(* fonction xor *)
let xor a b =
  if a = true then
    not b
  else
    b


let reset () =
  rx := -40.;
  ry := 0.;
  rz := 0.;
  tx := dtx();
  ty := dty();
  tz := dtz()


let motion ~x ~y =
  if !bl_down then
    begin
      anglex := !anglex + (!xold - x);
      angley := !angley + (!yold -y);
      Glut.postRedisplay();
    end;
  xold := x;
  yold := y



let mouse_event ~button ~state ~x ~y = match button, state with
  | Glut.LEFT_BUTTON, Glut.DOWN -> bl_down := true;
                                        xold := x;
                                        yold := y;
  | Glut.LEFT_BUTTON, Glut.UP -> bl_down := false;
  | _ -> ()


(* gestion des evenements du clavier *)
let keyboard_event ~key ~x ~y = match key with
    (* ESCAPE *)
    027 -> exit 0
  | 105 | 151  -> rx := !rx +. !pas
  | 107 | 153 -> rx:= !rx -. !pas
  | 106 | 152 -> ry := !ry +. !pas
  | 108 | 154 -> ry := !ry -. !pas
  | 117 | 165 -> rz := !rz -. !pas
  | 111 | 157 -> rz := !rz +. !pas
  | 119 | 167 -> mode_ := Line
  | 102 | 146 -> mode_ := Fill
  | 112 | 160 -> mode_ := Point
  | 113 | 161 -> tz := !tz +. !pas
  | 101 | 145 -> tz := !tz -. !pas
  | 114 | 162 -> reset ()
  | 50 -> ly := !ly -. !pas
  | 51 -> lz := !lz -. !pas
  | 52 -> lx := !lx -. !pas
  | 54 -> lx := !lx +. !pas
  | 56 -> ly := !ly +. !pas
  | 57 -> lz := !lz +. !pas
  | 103 -> light_b := (xor !light_b true)
  | _ -> ()

let keyboard_special_event ~key ~x ~y = match key with
  | Glut.KEY_LEFT -> tx := !tx -. !pas
  | Glut.KEY_RIGHT -> tx := !tx +. !pas
  | Glut.KEY_DOWN -> ty := !ty -. !pas
  | Glut.KEY_UP -> ty := !ty +. !pas
  | _ -> ()


(* fonction d'idle *)
let idle () =
  (*init_light ();*)
  Glut.postRedisplay();
  scene_gl ()


let main_engine () =
    (* init - pas dans la boucle *)
    init();
    (* gestion du clavier *)
    Glut.keyboardFunc keyboard_event;
    Glut.specialFunc keyboard_special_event;
    Glut.mouseFunc mouse_event;
    Glut.motionFunc motion;
    Glut.reshapeFunc reshape;
    Glut.idleFunc(Some idle);
    Glut.mainLoop ()

