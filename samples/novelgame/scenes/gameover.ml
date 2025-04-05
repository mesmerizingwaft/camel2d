open Camel2d

let load_resources =
  Resource.return ()

type t = unit

let init _ = ()

let sound_mixer t = SoundMixer.return t

let renderer _ =
  let open Renderer in
  let* sw, sh = get_canvas_size in
  set_font_size 50
  >> set_color (RGB (255, 255, 255))
  >> Text.draw_centerized ~x:(sw / 2) ~y:(sh / 2) "Game Over"

let updater t = Updater.return t

let event_handler _ t = Updater.return t

