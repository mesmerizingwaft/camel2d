
module type Scene = Camel2d_scene.T

type t = {
  scenes: (string, (module Scene)) Hashtbl.t;
  width: int;
  height: int;
}

let create ?(width=640) ?(height=480) () =
  let scenes = Hashtbl.create 10 in
  { scenes; width; height }

let add_scene (game: t) name scene =
  Hashtbl.add game.scenes name scene