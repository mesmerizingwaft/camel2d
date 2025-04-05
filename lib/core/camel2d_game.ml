
module type Scene = Camel2d_scene.T

type t = {
  scenes: (string, (module Scene)) Hashtbl.t
}

let create () =
  let scenes = Hashtbl.create 10 in 
  { scenes }

let add_scene (game: t) name scene =
  Hashtbl.add game.scenes name scene

let find_scene (game: t) name =
  Hashtbl.find game.scenes name