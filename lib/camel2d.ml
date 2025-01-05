open Js_of_ocaml

module type Scene = Camel2d_scene.T

module Promise = Promise
module Game = Camel2d_game
module Entity = Camel2d_entity
module Arbitrator = Camel2d_arbitrator
module Resource = Camel2d_resource
module ResourceUtils = Camel2d_resource_utils
module Event = Camel2d_event
module EventHandler = Camel2d_eventhandler
module SNSUtils = Camel2d_snsutils

let load_new_scene scene_name = Camel2d_decision.LoadScene scene_name
let update_entities entities = Camel2d_decision.Update entities

let _render (context: Camel2d_context.t) (world: Camel2d_world.t) =
  Camel2d_context.cleanup_canvas context;
  Camel2d_world.render context world

let _main_loop (context: Camel2d_context.t) scenes (world: Camel2d_world.t) =
  let rec frame world =
    let time_start = Js.date##now in
    let world =
      let open Promise in
      world
      |> Camel2d_world.handle_events scenes
      >>= Camel2d_world.arbitrate scenes
    in
    Promise.continue_after_resolved world (fun world ->
      _render context world;
      let time_consumed = Js.date##now -. time_start in
      let next_timing = max (15. -. time_consumed) 0. in
      ignore @@ Dom_html.window##setTimeout (Js.wrap_callback (fun () -> frame world)) next_timing
    )
  in
  frame world

let _init context (t: Game.t) =
  Camel2d_event.init context ();
  context.html_canvas##.width := t.width;
  context.html_canvas##.height := t.height

let start (t: Game.t) (scene_name: string) =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    let context = Camel2d_context.create () in
    let scene = Camel2d_game.fetch_scene t scene_name in
    let world = Camel2d_world.load_scene scene in
    _init context t;
    Promise.continue_after_resolved world (fun world ->
      _main_loop context t.scenes world
    );
    Js._false
  )
