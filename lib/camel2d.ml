open Js_of_ocaml

module Game = Camel2d_game
module Scene = Camel2d_scene
module World = Camel2d_world
module Event = Camel2d_event
module Resource = Camel2d_resource
module Entity = Camel2d_entity
module RenderableUtils = Camel2d_renderable_utils
module Templates = Camel2d_template

let _event_loop event_handler =
  fun context ->
  let rec inner () =
    let open World in
    match Event.take () with
      | None -> return ()
      | Some ev ->
        event_handler context ev
        >> inner ()
  in inner ()

let _load_new_scene context scenes name =
  let module S = (val Hashtbl.find scenes name : Scene.T) in
  let open Promise in
  Resource.Audio.stop_all ();
  Scene.load_resources context (module S) >>= fun bucket ->
  let state = World.new_state bucket in
  let initializor =
    let is_initialized = ref false in fun context ->
    if not !is_initialized then
      let res = S.initialize context in
      is_initialized := true;
      res
    else Camel2d_world.return ()
  in
  let updator = S.update in
  let event_handler = _event_loop S.handle_event in
  return (state, initializor, updator, event_handler)

let render context =
  let open World in
  let* bucket = get_bucket in
  let* renderables = get_renderables in
  Camel2d_context.cleanup_canvas context;
  List.iter (Camel2d_entity.Renderable.render context bucket) renderables;
  dbg_show_renderables
  >> return ()

let play_se _context =
  let open World in
  let* bucket = get_bucket in
  let* playables = get_playables in
  let playables =
    List.map (Entity.Playable.update bucket) playables
  in
  put_playables playables

let main context scenes initializor updator event_handler state =
  let rec inner initialize update handle_event state =
    let time_start = Js.date##now in
    let continue state =
      let time_consumed = Js.date##now -. time_start in
      let next_timing = max (15. -. time_consumed) 0. in
      let callback = Js.wrap_callback (fun () -> inner initialize update handle_event state) in
      ignore @@ Dom_html.window##setTimeout callback next_timing
    in
    let open World in
    match run ~state (
      initialize context
      >> handle_event context
      >> update context
      >> play_se context
      >> render context
    ) with
      | NewScene name ->
        let p_state = _load_new_scene context scenes name in
        Promise.continue_after_resolved p_state (fun (state, initializor, updator, event_handler) ->
          inner initializor updator event_handler state
        )
      | Continue (_, state) -> continue state
  in
  inner initializor updator event_handler state

let _init context (t: Game.t) =
  Camel2d_event.init context ();
  context.html_canvas##.width := t.width;
  context.html_canvas##.height := t.height

let start (t: Game.t) (scene_name: string) =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    let context = Camel2d_context.create () in
    _init context t;
    let p_state = _load_new_scene context t.scenes scene_name in
    Promise.continue_after_resolved p_state (fun (state, initializor, updator, event_handler) ->
      main context t.scenes initializor updator event_handler state
    );
    Js._false
  )