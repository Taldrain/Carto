(* Parse des arguments entrés en parametres du programme *)

let args =
  [("-help", Arg.Unit (fun () -> ()), "Blabla - todo")]
(* TODO ajouter des arguments *)

let parse () =
  Arg.parse args (fun "" -> ())(*...*) "SuperMap"
