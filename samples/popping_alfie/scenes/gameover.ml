open Camel2d

let literals = Literals.t

module ResourceLabels = struct
  open Resource
  let bg_gameover1 = gen_label ()
  let bgm_gameover = gen_label ()
end

type t = {
  play_se: bool;
  bg: Preset.Basic.Image.t;
  label_gameover: Preset.Basic.Text.t;
  label_score: Preset.Basic.Text.t;
  btn_tweet: Preset.Basic.Text.t;
  btn_retry: Preset.Basic.Text.t;
}

let load_resources =
  let open Resource in
  let open ResourceLabels in
  set_audio_root "/samples/popping_alfie/static/audio/"
  >> set_audio_mode SE
  >> load_audio bgm_gameover "iwa_gameover010.mp3"
  >> set_image_root "/samples/popping_alfie/static/imgs/"
  >> load_image bg_gameover1 "gameover1.png"

let init context =
  let cw, ch = Context.get_canvas_size context in
  let open Preset.Basic.Text in
  let style = let open Style in
    create ()
    |> set_color (RGB (255, 255, 255))
    |> set_outline (NoOutline)
  in
  let style_label = let open Style in style |> set_font_size 70 in
  let style_label_score = let open Style in style |> set_font_size 30 in
  let style_btn = let open Style in style |> set_font_size 20 |> set_color (RGB (200, 200, 200)) |> set_color_mouse_on (RGB (255, 255, 255)) in
  let label_gameover = create_centerized ~x:(cw / 2) ~y:(60) ~style:style_label "GAMEOVER" in
  let label_score = create_centerized ~x:(cw / 3 * 2 + 40) ~y:(150) ~style:style_label_score ("SCORE: " ^ string_of_int !Global.score) in
  let btn_tweet = create_centerized ~x:(cw / 3 * 2 + 40) ~y:(ch - 130) ~style:style_btn literals.label_tweet in
  let btn_retry = create_centerized ~x:(cw / 3 * 2 + 40) ~y:(ch - 100) ~style:style_btn literals.label_retry in
  let bg = Preset.Basic.Image.create_wh ~x:0 ~y:0 ~w:cw ~h:ch ResourceLabels.bg_gameover1 in
  {
    play_se = true;
    label_gameover;
    label_score;
    btn_tweet;
    btn_retry;
    bg
  }
let renderer t =
  let open Renderer in
  Preset.Basic.Image.render t.bg
  >> Preset.Basic.Text.render t.label_gameover
  >> Preset.Basic.Text.render t.label_score
  >> Preset.Basic.Text.render t.btn_tweet
  >> Preset.Basic.Text.render t.btn_retry

let sound_mixer t =
  let open SoundMixer in
  if t.play_se then begin
    play_sound ResourceLabels.bgm_gameover
    >> return {t with play_se = false}
  end else return t

let updater t =
  let open Updater in
  if Preset.Basic.Text.clicked t.btn_tweet then begin
    SNSUtils.tweet
      (literals.tweet_body ^ string_of_int !Global.score)
      ~url:(Some literals.url);
    return {t with btn_tweet = Preset.Basic.Text.unclick t.btn_tweet}
  end else if Preset.Basic.Text.clicked t.btn_retry then begin
    start_scene "main"
  end else return t

let event_handler e t =
  let open Updater in
  let* btn_retry = Preset.Basic.Text.handle_event e t.btn_retry in
  let* btn_tweet = Preset.Basic.Text.handle_event e t.btn_tweet in
  return {t with btn_retry; btn_tweet}
