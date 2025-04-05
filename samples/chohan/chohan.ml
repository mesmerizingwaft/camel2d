open Camel2d

let _ = Random.self_init ()
let literals = Literals.t

module AudioAgreement = Preset.Scene.AudioAgreement.Make(struct let next_scene = "main" end)

module GameMain : Scene.T = struct
  module ResourceLabels = struct
    open Resource
    let font_tamanegi = gen_label ()
    let bg, fg = gen_label (), gen_label ()
    let bgm1, bgm2 = gen_label (), gen_label ()
    let se_win, se_lose = gen_label (), gen_label ()
  end

  type state_init = {
    speech: Preset.Basic.Text.t;
    button_cho: Preset.Basic.Text.t;
    button_han: Preset.Basic.Text.t;
  }

  type hand = Cho | Han

  type state_hand_selected = {
    hand: hand;
    label_dice1: Preset.Basic.Text.t;
    label_dice2: Preset.Basic.Text.t;
    counter: int;
  }

  type state_game_end = {
    result: Preset.Animation.PopupText.t;
    label_dice1: Preset.Basic.Text.t;
    label_dice2: Preset.Basic.Text.t;
  }

  type game_phase =
    | Init of state_init
    | HandSelected of state_hand_selected
    | GameEnd of state_game_end

  type t = {
    game_phase: game_phase;
    stop_audio: bool;
    audios_to_be_played: Resource.label list;
  }
  let load_resources =
    let open Resource in
    let open ResourceLabels in
    set_image_root "static/imgs/"
    >> load_image bg "bg.jpg"
    >> load_image fg "tsubofurishi.png"
    >> set_font_root "static/fonts/"
    >> load_font font_tamanegi "tamanegi_v7.ttf"
    >> set_audio_root "static/audio/"
    >> set_audio_mode BGM
    >> load_audio bgm1 "bgm1.mp3"
    >> load_audio bgm2 "bgm2.mp3"
    >> set_audio_mode SE
    >> load_audio se_lose "lose.mp3"
    >> load_audio se_win "win.mp3"

  let bg = Preset.Basic.Image.create ~x:0 ~y:0 ResourceLabels.bg
  let fg = Preset.Basic.Image.create ~x:0 ~y:0 ResourceLabels.fg

  let style_base = let open Preset.Basic.Text.Style in
    create ()
    |> set_fontface ResourceLabels.font_tamanegi
    |> set_color (RGB (255, 255, 255))
    |> set_outline (Edging (RGB (0, 0, 0)))

  let style_speech = let open Preset.Basic.Text.Style in
    style_base |> set_font_size 25

  let style_button = let open Preset.Basic.Text.Style in
    style_base |> set_font_size 100 |> set_color_mouse_on (RGB (255, 200, 200))

  let init context = 
    let sw, sh = Context.get_canvas_size context in {
    stop_audio = false;
    audios_to_be_played = [ResourceLabels.bgm1];
    game_phase =
      let open Preset.Basic in
      Init {
        speech = Text.create_centerized ~style:style_speech ~x:(sw / 2) ~y:20 literals.speech;
        button_cho = Text.create_centerized ~style:style_button ~x:(sw / 3) ~y:(sh/2) literals.cho;
        button_han = Text.create_centerized ~style:style_button ~x:(sw / 3 * 2) ~y:(sh/2) literals.han;
      }
    }
  let renderer t =
    let open Renderer in
    Preset.Basic.Image.(render bg >> render fg)
    >> match t.game_phase with
      | Init {speech; button_cho; button_han} ->
        let open Preset.Basic.Text in
        render speech >> render button_cho >> render button_han
      | HandSelected { label_dice1; label_dice2; _ } ->
        let open Preset.Basic.Text in
        render label_dice1 >> render label_dice2
      | GameEnd { label_dice1; label_dice2; result } ->
        Preset.Basic.Text.render label_dice1
        >> Preset.Basic.Text.render label_dice2
        >> Preset.Animation.PopupText.render result

  let sound_mixer t =
    let open SoundMixer in
    let rec inner = function
      | [] -> return {t with audios_to_be_played = []; stop_audio=false}
      | label :: rest ->
        play_sound label >> inner rest
    in
    (if t.stop_audio then print_endline "stop_audio" else return ())
    >> (if t.stop_audio then stop_all_sounds else return ())
    >> inner t.audios_to_be_played

  let choose_hand hand =
    let open Updater in
    let open Preset.Basic.Text in
    let* cw, ch = get_canvas_size in
    let style = let open Style in
      create ()
      |> set_font_size 100
      |> set_color (RGB (255, 255, 255))
      |> set_outline (Edging (RGB (0, 0, 0)))
    in
    let label_dice1 = create_centerized ~style ~x:(cw / 3) ~y:(ch / 2) literals.dice_face.(0) in
    let label_dice2 = create_centerized ~style ~x:(cw / 3 * 2) ~y:(ch / 2) literals.dice_face.(0) in
    return (HandSelected {
        hand; label_dice1; label_dice2; counter=0
    })

  let update_in_init t e state_init =
    let open Updater in
    let* button_cho = Preset.Basic.Text.update e state_init.button_cho in
    let* button_han = Preset.Basic.Text.update e state_init.button_han in
    match e with
      | Event.Tick when Preset.Basic.Text.(clicked button_cho || clicked button_han) ->
        let hand = if Preset.Basic.Text.clicked button_cho then Cho else Han in
        let* game_phase = choose_hand hand in
        return {
          stop_audio = true;
          audios_to_be_played = [ResourceLabels.bgm2];
          game_phase
        }
      | _ -> return {t with game_phase = Init {
        state_init with button_cho; button_han
      }}

  let update_in_hand_selected t e (state_hand_selected: state_hand_selected) =
    let open Updater in
    let* cw, ch = get_canvas_size in
    let* label_dice1 = Preset.Basic.Text.update e state_hand_selected.label_dice1 in
    let* label_dice2 = Preset.Basic.Text.update e state_hand_selected.label_dice2 in
    let counter = state_hand_selected.counter in
    let hand = state_hand_selected.hand in
    match e with
      | Event.Tick ->
        let dice1, dice2 = Random.int 6, Random.int 6 in
        let label_dice1 = Preset.Basic.Text.update_text (literals.dice_face.(dice1)) label_dice1 in
        let label_dice2 = Preset.Basic.Text.update_text (literals.dice_face.(dice2)) label_dice2 in        
        if counter <= 60 then begin
          return {t with game_phase = HandSelected {
            state_hand_selected with
            counter = counter + 1;
            label_dice1;
            label_dice2
          }}
        end else begin
          let message, se =
          match hand, (dice1 + dice2) mod 2 = 0 with
            | Cho, true | Han, false -> literals.win, ResourceLabels.se_win
            | Cho, false | Han, true -> literals.lose, ResourceLabels.se_lose
          in
          let result =
            let open Preset.Animation.PopupText in
            let style = let open Style in style_base |> set_font_size 200 in
            let f t = if t <= 10 then Some (200 + (5 - t) * 10) else Some 200 in
            let t = create_centerized ~style ~x:(cw/2) ~y:(ch/2) message in
            create ~f t
          in
          return {
            stop_audio = true;
            audios_to_be_played = [se];
            game_phase = GameEnd {
              result;
              label_dice1;
              label_dice2;
          }}
        end
      | _ -> return t

  let update_in_game_end t e (state_game_end: state_game_end) =
    let open Updater in
    let* result = Preset.Animation.PopupText.update state_game_end.result in
    match e with
      | Event.MouseUp _ -> start_scene "main"
      | _ -> return {t with game_phase = GameEnd {
        state_game_end with result
      }}

  let updater e t =
    match t with
      | {game_phase = Init state_init; _} ->
        update_in_init t e state_init
      | {game_phase = HandSelected state_hand_selected; _} ->
        update_in_hand_selected t e state_hand_selected
      | {game_phase = GameEnd state_game_end; _} ->
        update_in_game_end t e state_game_end

end

let _ = 
  let chohan = Game.create () in
  Game.add_scene chohan "audio_agreement" (module AudioAgreement);
  Game.add_scene chohan "main" (module GameMain);
  start chohan "audio_agreement"
