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

let update t =
  let open Updater in
  let* t = fall t in
  let* t = update_img t in
  let jumpable = if t.h = 0 then true else false in
  return {t with jumpable}

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

(*
open Camel2d
open Entity

include SingleImage

module ResourceLabels = struct
  open Resource
  let alfie_steady = gen_label ()
  let alfie_in_air = gen_label ()
end

module Id = struct
  let alfie_steady = "alfie_steady"
  let alfie_in_air = "alfie_in_air"
end

let load_resources () =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/popping_alfie/static/imgs/"
  >> load_image alfie_steady "alfie1.png"
  >> load_image alfie_in_air "alfie2.png"

let create ground_height =
  let h = ref 0 in
  let init _context =
    let pos = 0, 0 in
    let alfie_steady =
      let size = 80, 50 in
      create ~pos ~size ~is_visible:true Id.alfie_steady ResourceLabels.alfie_steady
    in
    let alfie_in_air =
      let size = 90, 50 in
      create ~pos ~size ~is_visible:false Id.alfie_in_air ResourceLabels.alfie_in_air
    in
    let open World in
    spawn [alfie_steady; alfie_in_air]
  in
  let updator =
    let open World in
    let* h = use_ref h in
    update_when Condition.(has_id Id.alfie_steady ||| has_id Id.alfie_in_air) Updator.(update_y (ground_height - h))
    >> let for_alfie_steady = update_when Condition.(has_id Id.alfie_steady) in
    let for_alfie_in_air = update_when Condition.(has_id Id.alfie_in_air) in
    if h > 0 then begin
      for_alfie_steady Updator.hide
      >> for_alfie_in_air Updator.show
    end else begin
      for_alfie_steady Updator.show
      >> for_alfie_in_air Updator.hide
    end
  in
  init, updator, h
  *)