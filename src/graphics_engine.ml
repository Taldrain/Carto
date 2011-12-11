(* Graphics engine *)


(* --- Variable --- *)

(* mouse var *)
let xold = ref 0.
let yold = ref 0.
let bl_down = ref false
let zold = ref 0.
let rzold = ref 0.
let br_down = ref false

(* for the movement of the map with keys *)
let pas = ref 2.

(* display mode the the map *)
type mode = Fill | Line | Point
let mode_ = ref Line

let g_mode = function
  | Fill -> `fill
  | Line -> `line
  | Point -> `point

let fullscreen = ref true

(* rotation var *)
let rx = ref (-40.)
let ry = ref 0.
let rz = ref 0.

(* translation var *)
let dtx() = (float (-(Refe.get_w()/(2*Refe.get_step()))))
let tx = ref 0.
let dty() = (float (-(Refe.get_w()/(4*Refe.get_step()))))
let ty = ref 0.
let dtz() = (float (-((Refe.get_h())/Refe.get_step())))
let tz = ref 0.

(* light var *)
let lx = ref 0.
let ly = ref (-242.)
let lz = ref 0.
let monte = ref true
let light_b = ref true

let init_b = ref true
let set_init ()= init_b := true
let get_init ()= !init_b



(* --- Function --- *)

let reset () =
  rx := -40.;
  ry := 0.;
  rz := 0.;
  tx := dtx();
  ty := dty();
  tz := dtz()

let init () =
  ignore (Glut.init Sys.argv);
  Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true ();
  (* init of some variable translation *)
  reset ();
  ly := (-242.);
  monte := true;
  (* color gradient *)
  GlDraw.shade_model `smooth;
  (* background color *)
  GlClear.color (0., 0., 0.);
  GlClear.depth 1.;
  List.iter Gl.enable [ `depth_test;
                        `lighting;
                        `light0;
                        `color_material;];
  (* for the light *)
  GlLight.color_material ~face:`front `ambient_and_diffuse;
  GlLight.light_model (`ambient (0.2, 0.2, 0.2, 1.));
  GlDraw.shade_model `smooth;
  GlFunc.depth_func `less;
  GlMisc.hint `perspective_correction `nicest


let init_light () =
  let light_am = 0., 0., 0., 1.
  and light_diffuse = 1., 1., 1., 1. (*!lr*.2., !lg*.2., !lb*.2., 1.*)
  and light_specular = 1., 1., 1., 1. (*!lr*.4., !lg*.4., !lb*.4., 1*)
  and light_position = !lx, !ly, !lz, 1.
  in
  List.iter (GlLight.light ~num:0)
    [ `ambient light_am;
      `diffuse light_diffuse;
      `specular light_specular;
      `position light_position ];
  GlLight.material ~face:`front (`specular (0.5, 0.5, 0.5, 1.));
  GlLight.material ~face:`front (`emission (0., 0., 0., 1.));
  GlLight.material ~face:`front (`shininess 42.0)


let dn () =
  if !monte then
    if (!ly < 381.) then
      ly := !ly +. 4.
    else
      monte := false
  else
    if (!ly > (-243.)) then
      ly := !ly -. 4.
    else
      monte := true


let gColor = function
  | (r,g,b) -> GlDraw.color ((r/.255.), (g/.255.), (b/.255.))


let rec create_tri = function
    [] -> ()
  | e::l -> gColor (snd e);
            GlDraw.vertex3 (fst e);
            create_tri l


(* affichage de la scene - display *)
let display (*~area*) ()  =
  (* init de init, et ouais, norag' *)
  if !init_b then
    (init ();
    init_b := false);
  GlClear.clear [`color; `depth];
  GlMat.mode `modelview;
  GlMat.load_identity ();
  init_light ();
  GlMat.translate3 (!tx, !ty, !tz);
  GlMat.rotate3 !rx (2.0, 0.0, 0.0);
  GlMat.rotate3 !ry (0.0, 2.0, 0.0);
  GlMat.rotate3 !rz (0.0, 0.0, 2.0);
  GlDraw.polygon_mode `both (g_mode !mode_);
  GlDraw.line_width 1.0;
  GlDraw.begins `triangles;
  create_tri (Refe.get_list_tri3D());
  GlDraw.ends ();
  (* reshape *)
  let ratio = (float_of_int 1024) /. (float_of_int 768) in
    GlMat.mode `projection;
    GlMat.load_identity ();
    GlDraw.viewport 0 0 1024 768;
    GluMat.perspective ~fovy:45. ~aspect:ratio ~z:(1.,500.);
  Gl.flush ()


let xor a b =
if a = true then
  not b
else
  b



(* deal with the mouse event *)
let motion x y =
  if !bl_down then
    begin
      tx := (!tx -. (!xold -. x));
      ty := (!ty +. (!yold -. y));
    end;
  if !br_down then
    begin
      tz := (!tz +. (!zold -. y));
      rz := (!rz -. (!rzold -. x));
    end;
  xold := x;
  zold := y;
  yold := y;
  rzold := x


(* mouse event *)
let act_leftDown x y = bl_down := true; xold := x; yold := y
let act_leftUp () = bl_down := false
let act_rightDown x y = br_down := true; zold := y; rzold := x
let act_rightUP () = br_down := false


(* keyboard event *)
let act_i () = rx := !rx +. !pas
let act_k () = rx := !rx -. !pas
let act_j () = ry := !ry +. !pas
let act_l () = ry := !ry -. !pas
let act_u () = rz := !rz -. !pas
let act_o () = rz := !rz +. !pas
let act_w () = mode_ := Line
let act_f () = mode_ := Fill
let act_p () = mode_ := Point
let act_q () = tz := !tz +. !pas
let act_e () = tz := !tz -. !pas
let act_r () = reset ()
let act_g () = light_b := (xor !light_b true)
let act_2 () = ly := !ly -. !pas
let act_3 () = lz := !lz -. !pas
let act_4 () = lx := !lx -. !pas
let act_6 () = lx := !lx +. !pas
let act_8 () = ly := !ly +. !pas
let act_9 () = lz := !lz +. !pas


(* special keyboard event *)
let act_keyLeft () = tx := !tx -. !pas
let act_keyRight () = tx := !tx +. !pas
let act_keyDown () = ty := !ty -. !pas
let act_keyUP () = ty := !ty +. !pas


