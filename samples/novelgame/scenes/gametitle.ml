open Camel2d

module ResourceLabels = struct
  open Resource
  let bg = gen_label ()
  let font_bebas = gen_label ()
end

type t = {
  bg: Preset.Basic.Image.t;
  title: Preset.Basic.Text.t;
}

let load_resources =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/novelgame/static/imgs/"
  >> load_image bg "title.png"
  >> set_font_root "/samples/novelgame/static/fonts/"
  >> load_font font_bebas "BebasNeue-Regular.ttf"

let init context =
  let sw, sh = Context.get_canvas_size context in
  let bg =
    let open Preset.Basic.Image in
    create_wh ~x:0 ~y:0 ~w:sw ~h:sh ResourceLabels.bg
  in
  let title =
    let open Preset.Basic.Text in
    let style = Style.create ()
    |> Style.set_color (RGB (255, 255, 255))
    |> Style.set_fontface ResourceLabels.font_bebas
    |> Style.set_font_size 50
    |> Style.set_outline (Edging (RGBA (0, 0, 0, 1.)))
    in
    create_centerized ~x:(sw / 2) ~y:(sh / 2 - 100) ~style "TITLE (fontface Bebas)"
  in
  { bg; title}

let sound_mixer t =
  let open SoundMixer in
  return t

let renderer t =
  let open Renderer in
  Preset.Basic.Image.render t.bg
  >> Preset.Basic.Text.render t.title

let updater e t =
  let open Updater in
  match e with
    | Event.MouseDown _ | KeyDown {key_code = 32} ->
      start_scene "prologue"
    | _ -> return t
