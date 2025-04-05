open Camel2d

module ResourceLabels = struct
  open Resource
  let bgm = gen_label ()
  let se_jump = gen_label ()
end

type t = {
  cnt: int;
  score: int;
  bg: Bg.t;
  fg: Fg.t;
  jump_power: int;
  alfie: Alfie.t;
  items: Items.t;
  se_to_be_played: Resource.label list;
}

let load_resources =
  let open Resource in
  let open ResourceLabels in
  Bg.load_resources
  >> Fg.load_resources
  >> Alfie.load_resources
  >> Items.load_resources
  >> set_audio_root "/samples/popping_alfie/static/audio/"
  >> set_audio_mode SE
  >> load_audio se_jump "hyuun.mp3"
  >> set_audio_mode BGM
  >> load_audio bgm "iwashiro_kokage_biyori.mp3"

let init context =
  let sw, sh = Context.get_canvas_size context in
  let ground_height = sh - 100 in
  let jump_power = 0 in
  let alfie = Alfie.create ground_height in
  let bg = Bg.create sw sh in
  let fg = Fg.create sw sh in
  let items = Items.create sw sh in
  let score = 0 in
  let se_to_be_played = [ResourceLabels.bgm] in
  let cnt = 0 in
  {cnt; score; fg; bg; jump_power; alfie; items; se_to_be_played}
let renderer t =
  let open Renderer in
  Bg.render t.bg
  >> Fg.render t.fg
  >> Items.render t.items
  >> Alfie.render t.alfie


let sound_mixer t =
  let open SoundMixer in
  let rec inner = function
    | [] -> return ()
    | x :: xs ->
      play_sound x
      >> inner xs
  in
  inner t.se_to_be_played
  >> return {t with se_to_be_played = []}

let jump t =
  let jump_power_to_v_h jp = (jp / 2 + 1) * 6 in
  let alfie = Alfie.jump (jump_power_to_v_h t.jump_power) t.alfie in
  let jump_power = 0 in
  {t with jump_power; alfie; se_to_be_played = [ResourceLabels.se_jump]}

let updater t =
  let open Updater in
  let jump_max = 5 in
  let t = {t with cnt = (t.cnt + 1) mod Int.max_int } in
  let* bg = Bg.update t.bg in
  let* fg = Fg.update t.fg in
  let* items = Items.update t.items in
  let* alfie = Alfie.update t.alfie in
  let item_obtained, items = Items.get_item ~f:(
    fun x y w h -> Alfie.check_collision x y w h alfie
  ) items in
  let score = match item_obtained with
    | Some (Items.Chamoile) -> t.score + 10
    | _ -> t.score
  in
  if item_obtained = Some (Items.Strawberry) then begin
    Global.score := score;
    start_scene "gameover"
  end else
    let t = {t with score; bg; fg; items; alfie} in
    if t.jump_power > jump_max
    then return (jump t)
    else begin
      let jump_power =
        if t.jump_power > 0 then t.jump_power + 1 else t.jump_power
      in
      return {t with jump_power}
    end
let event_handler e t =
  let open Updater in
  match e with
    | Event.KeyDown {key_code = 32} when Alfie.jumpable t.alfie ->
      let jump_power = if t.jump_power = 0 then 1 else 0 in
      return {t with jump_power}
    | Event.KeyUp {key_code = 32} when t.jump_power > 0 ->
      return (jump t)
    | _ -> return t


