open Js_of_ocaml

module Event = Camel2d_event
module Resource = Camel2d_resource
module Updater = Camel2d_updater
module SoundMixer = Camel2d_sound_mixer
module Scene = Camel2d_scene
module Preset = Camel2d_preset
module Game = Camel2d_game
module Renderer = Camel2d_renderer
module Context = Camel2d_context
module SNSUtils = Camel2d_snsutils
module Unicode = Unicode

type color = Camel2d_color.t

let _event_loop event_handler t =
  let rec inner t =
    let open Updater in
    match Event.take () with
      | None -> return t
      | Some ev ->
        let* t' = event_handler ev t in
        inner t'
  in inner t

let rec _start_scene
  (context: Camel2d_context.t)
  (game: Game.t)
  (first_scene: string) =
  let module S = (val Game.find_scene game first_scene) in
  let event_loop = _event_loop S.updater in
  let model = S.init context in
  Resource.Audio.stop_all ();
  let rec main_loop bucket state (model: S.t) =
    let time_start = Js.date##now in
    let continue bucket state model =
      let time_consumed = Js.date##now -. time_start in
      let next_timing = max (Camel2d_global.mspf -. time_consumed) 0. in
      let callback = Js.wrap_callback (fun () -> main_loop bucket state model) in
      ignore @@ Js_of_ocaml.Dom_html.window##setTimeout callback next_timing
    in
    let open Updater in
    let next_scene =
      Camel2d_error.error_presenter (fun () ->
      Updater.run ~state (event_loop model >>= S.updater Event.Tick >>= S.sound_mixer)
      )
    in
    match next_scene with
      | NewScene name -> _start_scene context game name
      | Continue (model, state) ->
        let r_state = Renderer.new_state context bucket in
        Camel2d_context.cleanup_canvas context;
        ignore @@ Renderer.run ~state:r_state (S.renderer model);
        continue bucket state model
  in
  let to_be_bucket = Scene.load_resources context (module S) in
  Promise.continue_after_resolved to_be_bucket (fun bucket ->
    let state = Updater.new_state context bucket in
    main_loop bucket state model
  )

(** Start the game from the first_scene
  @param width The width of the game window. (default: 640)
  @param height The height of the game window. (default: 480)
*)
let start
  ?(width=640)
  ?(height=480)
  (game: Game.t) (first_scene: string): unit =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    let context = Camel2d_context.create () in
    Camel2d_context.set_canvas_size context width height;
    Camel2d_event.init context ();
    _start_scene context game first_scene;
    Js._false
  )