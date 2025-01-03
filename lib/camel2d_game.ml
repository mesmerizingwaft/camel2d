type t = {
  scenes: (string, (module Camel2d_scene.T)) Hashtbl.t;
  world: Camel2d_world.t;
  width: int;
  height: int;
}

let create () =
  let scenes = Hashtbl.create 10 in
  let world = Camel2d_world.create () in
  let width = 640 in
  let height = 480 in
  { scenes; world; width; height }

let add_scene ?(wait_loading=Camel2d_utils.no_loading_screen) t name scene =
  let scene = wait_loading scene in
  Hashtbl.add t.scenes name scene

let fetch_scene t scene_name =
  Hashtbl.find t.scenes scene_name

