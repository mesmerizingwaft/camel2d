open Camel2d

include Preset.Basic.Image
type _t = t

type t = {
  sw: int;
  imgs: _t list;
  counter: int;
}

module ResourceLabels = struct
  open Resource
  let fg = gen_label ()
end

let load_resources =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/popping_alfie/static/imgs/"
  >> load_image fg "clouds.png"

let create sw sh =
  let imgs = [
    create_wh ~x:0 ~y:0 ~w:sw ~h:sh ResourceLabels.fg;
    create_wh ~x:sw ~y:0 ~w:sw ~h:sh ResourceLabels.fg;
  ] in
  {sw; imgs; counter=0}

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
  if t.counter / 5 = 0 then
    return {t with counter=t.counter+1}
  else
    let t = {t with counter=0} in
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
