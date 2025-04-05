open Camel2d

type t = {
  h: int;
  v_h: int;
  h_to_y: int -> int;
  jumpable: bool;
  fall_count: int;
  img_walking: Preset.Animation.AnimatedImage.t;
  img_air: Preset.Basic.Image.t;
}

module ResourceLabels = struct
  open Resource
  let alfie_walking = gen_label ()
  let alfie_in_air = gen_label ()
end

let load_resources =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/popping_alfie/static/imgs/"
  >> load_anime alfie_walking "alfie_running.gif"
  >> load_image alfie_in_air "alfie2.png"

let create ground =
  let h_to_y h = ground - h in
  let img_walking = let open Preset.Animation.AnimatedImage in
    create_wh ~x:0 ~y:0 ~w:93 ~h:70 ResourceLabels.alfie_walking
  in
  let img_air = let open Preset.Basic.Image in
    create_wh ~x:0 ~y:0 ~w:90 ~h:50 ResourceLabels.alfie_in_air
  in
  let h, v_h = 0, 0 in {
    h; v_h;
    h_to_y;
    img_walking;
    img_air;
    jumpable = true;
    fall_count=0;
  }

let render t =
  if t.h > 0 then
    Preset.Basic.Image.render t.img_air
  else
    Preset.Animation.AnimatedImage.render t.img_walking

let fall t =
  let open Updater in
  if t.fall_count > 1 then begin
    let g = 1 in
    let v_h = if t.h > 0 then t.v_h - g else t.v_h in
    let h = Int.max (t.h + v_h) 0 in
    return {t with h; v_h; fall_count = 0}
  end else return {t with fall_count = t.fall_count + 1}

let update_img t =
  let open Updater in
  let* img_walking = Preset.Animation.AnimatedImage.update t.img_walking in
  let img_walking = Preset.Animation.AnimatedImage.update_y (t.h_to_y t.h) img_walking in
  let img_air = let open Preset.Basic.Image in
    t.img_air
    |> update_y (t.h_to_y t.h)
    |> update_h 50
  in
  return {t with img_air; img_walking}

let update e t =
  let open Updater in
  match e with
    | Event.Tick ->
      let* t = fall t in
      let* t = update_img t in
      let jumpable = if t.h = 0 then true else false in
      return {t with jumpable}
    | _ -> return t

let jump v_h t = {t with v_h}

let jumpable t = t.jumpable

let check_collision x y w h t =
  let x', y', w', h' =
    if t.h > 0 then Preset.Basic.Image.(x_of t.img_air, y_of t.img_air, w_of t.img_air, h_of t.img_air)
    else Preset.Animation.AnimatedImage.(x_of t.img_walking, y_of t.img_walking, w_of t.img_walking, h_of t.img_walking)
  in
  let x1, y1, x2, y2 = x, y, x + w, y + h in
  let x1', y1', x2', y2' = x', y', x' + w', y' + h' in
  if x1' < x2 && x1 < x2' && y1' < y2 && y1 < y2' then true else false
