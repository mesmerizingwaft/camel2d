module Renderer = Camel2d_renderer
module Updater = Camel2d_updater
module Event = Camel2d_event
module Color = Camel2d_color

type t = {
  mouse_on: bool;
  is_clicked : bool;
  check_inclusion : int -> int -> bool;
}

let init = {
  mouse_on = false;
  is_clicked = false;
  check_inclusion = (fun _ _ -> false);
}

let render t =
  let open Renderer in
  let* (w, h) = get_canvas_size in
  let color = if t.mouse_on
    then Color.RGB (255, 255, 255)
    else RGB (120, 120, 120)
  in
  Symbol.draw_play_button ~x:(w / 2) ~y:(h / 2) ~radius:50 ~color

let update t =
  let open Updater in
  let* (w, h) = get_canvas_size in
  let (x, y) = (w / 2, h / 2) in
  let check_inclusion x' y' =
    let dx = x - x' in
    let dy = y - y' in
    dx * dx + dy * dy < 50 * 50
  in
  return {t with check_inclusion}

let handle_event e t =
  let open Updater in
  match e with
    | Event.MouseUp {x; y; _} when t.check_inclusion x y ->
      resume_audio
      >> return {t with is_clicked = true}
    | Event.MouseMove {x; y} when t.check_inclusion x y ->
      return {t with mouse_on = true}
    | Event.MouseMove _ ->
      return {t with mouse_on = false}
    | _ -> return t
      
let clicked t = t.is_clicked