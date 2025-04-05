open Camel2d 

open Preset.Basic.Image

type _t = t
type t = {
  sw: int;
  imgs: _t list;
}

module ResourceLabels = struct
  open Resource
  let bg = gen_label ()
end

let load_resources =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/popping_alfie/static/imgs/"
  >> load_image bg "bg.jpg"

let create sw sh = {
  sw; imgs = [
    create_wh ~x:0 ~y:0 ~w:(sw+1) ~h:sh ResourceLabels.bg;
    create_wh ~x:sw ~y:0 ~w:(sw+1) ~h:sh ResourceLabels.bg;
  ]
}

let render t =
  let open Renderer in
  let rec inner = function
    | [] -> return ()
    | img :: imgs ->
      render img >>
      inner imgs
  in inner t.imgs

let update t =
  let open Updater in
  let rec inner = function
    | [] -> []
    | img :: imgs ->
      let x = x_of img in
      let img = update_x (x - 1) img in
      img::(inner imgs)
  in
  let imgs = inner t.imgs in
  let imgs =
    if x_of (List.hd imgs) < -t.sw
    then
      let img = List.hd imgs in
      let img = update_x t.sw img in
      (List.tl imgs)@[img]
    else imgs
  in
  return {t with imgs}