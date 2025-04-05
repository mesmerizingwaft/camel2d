open Camel2d

let literals = Literals.t

module ResourceLabels = struct
  open Resource
  let bgm = gen_label ()
  let bg = gen_label ()
  let font_mochiy_pop_one = gen_label ()
end

type t = {
  play_audio: bool;
  counter: int;
  show_msg: bool;
  bg: Preset.Basic.Image.t;
  fg: Fg.t;
  title_logo: Preset.Basic.Text.t;
  title_inst: Preset.Basic.Text.t;
}

let load_resources =
  let open Resource in
  let open ResourceLabels in
  Fg.load_resources
  >> set_audio_root "/samples/popping_alfie/static/audio/"
  >> set_audio_mode BGM
  >> load_audio bgm "bgm.mp3"
  >> set_image_root "/samples/popping_alfie/static/imgs/"
  >> load_image bg "bg.jpg"
  >> set_font_root "/samples/popping_alfie/static/fonts/"
  >> load_font font_mochiy_pop_one "MochiyPopOne-Regular.ttf"

let style_mochi_white = let open Preset.Basic.Text.Style in
  create ()
  |> set_fontface ResourceLabels.font_mochiy_pop_one
  |> set_color (RGB (255, 255, 255))
  |> set_outline (Edging (RGB (0, 0, 0)))

let init context =
  let open Preset in
  let sw, sh = Context.get_canvas_size context in
  let bg = Basic.Image.create_wh ~x:0 ~y:0 ~w:sw ~h:sh ResourceLabels.bg in
  let title_logo =
    let open Basic.Text in
    let style = let open Style in
      style_mochi_white |> set_font_size 50
    in 
    create_centerized ~style ~x:(sw / 2) ~y:(sh / 2 - 100) literals.title
  in
  let title_inst =
    let open Basic.Text in
    let style = let open Style in
      create ()
      |> set_font_size 20
      |> set_color (RGB (0, 0, 0))
      |> set_outline (Edging (RGB (255, 255, 255)))
    in
    create_centerized ~style ~x:(sw / 2) ~y:(sh / 2 + 100) literals.title_inst
  in
  let play_audio = true in
  let counter = 0 in
  let show_msg = true in
  let fg = Fg.create sw sh in
  {play_audio; counter; show_msg; bg; fg; title_logo; title_inst}

let renderer t =
  let open Renderer in
  let open Preset.Basic in
  Image.render t.bg
  >> Fg.render t.fg
  >> Text.render t.title_logo
  >> (if t.show_msg then Text.render t.title_inst else return ())

let sound_mixer t =
  let open SoundMixer in
  if t.play_audio then begin
    play_sound ResourceLabels.bgm
    >> return {t with play_audio = false}
  end else return t

let updater t =
  let open Updater in
  let* fg = Fg.update t.fg in
  if t.counter mod 60 = 0
  then return {t with fg; show_msg = not t.show_msg; counter=((t.counter + 1) mod Int.max_int)}
  else return {t with fg; counter=((t.counter + 1) mod Int.max_int)}

let event_handler e t =
  let open Updater in
  match e with
    | Event.MouseUp _ ->
      start_scene "main"
    | _ -> return t
