open Camel2d
open Preset.Basic.Image

type _t = t

type item =
  | Chamoile
  | Strawberry

let speed_of = function
  | Chamoile -> 3
  | Strawberry -> 5

type entity = {item: item; img: _t}
  
type t = {
  sw: int;
  sh: int;
  entities: entity list
}

module ResourceLabels = struct
  open Resource
  let chamoile = gen_label ()
  let strawberry = gen_label ()
end

let load_resources =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/popping_alfie/static/imgs/"
  >> load_image chamoile "flower_chamomile.png"
  >> load_image strawberry "fruit_strawberry.png"

let create sw sh = {
  sw; sh; entities = []
}

let render t =
  let open Renderer in
  print_endline (string_of_int (List.length t.entities))
  >>
  let rec inner = function
    | [] -> return ()
    | {img; _} :: xs ->
      render img
      >> inner xs
  in inner t.entities

let spawn_new_item t =
  let item = if Random.bool () then Chamoile else Strawberry in
  let h = 30 + Random.int (t.sh - 60) in
  let img = match item with
    | Chamoile -> create_wh ~x:t.sw ~y:h ~w:30 ~h:30 ResourceLabels.chamoile
    | Strawberry -> create_wh ~x:t.sw ~y:h ~w:30 ~h:30 ResourceLabels.strawberry
  in
  {t with entities = ({item; img} :: t.entities)}

let update_entities t =
  let open Updater in
  let rec inner = function
    | [] -> return []
    | {item; img} :: xs ->
      let* img = update img in
      let* xs = inner xs in
      return ({item; img} :: xs)
  in inner t.entities

let update t =
  let open Updater in
  let* entities = update_entities t in
  let t = {t with entities} in
  let t = if Random.float 1. < 0.01 then spawn_new_item t else t in
  let entities = List.filter_map (fun {item; img} ->
    let x = x_of img in
    if x < 0 then None
    else
      let img = img |> update_x (x - (speed_of item)) in
      Some {item; img}
  ) t.entities in
  return {t with entities}

let get_item ~f t =
  let entity = List.find_opt (fun {img; _} ->
    let x, y, w, h = x_of img, y_of img, w_of img, h_of img in
    f x y w h
  ) t.entities
  in
  match entity with
    | None -> None, t
    | Some entity ->
      let entities = List.filter (fun x -> x <> entity) t.entities in
      (Some entity.item), {t with entities}
