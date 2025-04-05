module Updater = Camel2d_updater
module Renderer = Camel2d_renderer
type context = Camel2d_context.t


module PopupText : sig
  include module type of Camel2d_preset_basic.Text
  type t
  val create: f:(int -> int option) -> Camel2d_preset_basic.Text.t -> t
  val update : t -> t Updater.t
  val render : t -> unit Camel2d_renderer.t
end = struct
  include Camel2d_preset_basic.Text
  type _t = t
  type t = {
    t: _t;
    counter: int;
    f: int -> int option;
  }

  let create ~f t = {t; counter = 0; f}

  let update u =
    let open Updater in
    let pt = u.f u.counter in
    match pt with
      | None -> return u
      | Some pt ->
        let counter = u.counter + 1 in
        let t = update_font_size pt u.t in
        return {u with t; counter}

  let render u = render u.t
end

module AnimatedImage : sig
  type t
  val render : t -> unit Renderer.t
  val update : t -> t Updater.t
  val create : x:int -> y:int -> Camel2d_resource.label -> t
  val create_wh : x:int -> y:int -> w:int -> h:int -> Camel2d_resource.label -> t
  val update_x : int -> t -> t
  val update_y : int -> t -> t
  val update_w : int -> t -> t
  val update_h : int -> t -> t
  val x_of : t -> int
  val y_of : t -> int
  val w_of : t -> int
  val h_of : t -> int
end = struct
  type t = {
    x: int;
    y: int;
    size: (int * int) option;
    label: Camel2d_resource.label;
    counter: int;
  }

  let update t =
    let open Updater in
    let msec = (float_of_int t.counter) *. Camel2d_global.spf *. 100. in
    let* (w, h) = get_anime_size t.label (int_of_float msec) in
    let size = if Option.is_none t.size then Some (w, h) else t.size in
    let counter = (t.counter + 1) mod Int.max_int in
    return {t with size; counter}

  let render t =
    let open Camel2d_renderer in
    match t.size with
      | None -> Anime.render_xy ~x:t.x ~y:t.y t.label t.counter
      | Some (w, h) -> Anime.render_xywh ~x:t.x ~y:t.y ~w ~h t.label t.counter

  let create ~x ~y label =
    {x; y; size = None; label; counter = 0}
  
  let create_wh ~x ~y ~w ~h label =
    {x; y; size = Some (w, h); label; counter = 0}

  let update_x x t = {t with x}
  let update_y y t = {t with y}
  let update_w w t =
    match t.size with
      | None -> failwith "To update width, you need to call update first"
      | Some (_, h) -> {t with size = Some (w, h)}
  let update_h h t =
    match t.size with
      | None -> failwith "To update height, you need to call update first"
      | Some (w, _) -> {t with size = Some (w, h)}
  let x_of t = t.x
  let y_of t = t.y
  let w_of t = match t.size with
    | None -> failwith "To get width, you need to call update first"
    | Some (w, _) -> w
  let h_of t = match t.size with
    | None -> failwith "To get height, you need to call update first"
    | Some (_, h) -> h
end

module FadingImage : sig
  type t

  val linear_fadeout : int -> int -> float
  val linear_fadein : int -> int -> float
  val linear_fadeinout : fadein_by:int -> fadeout_from:int -> frame_end:int -> int -> float
  val create : x:int -> y:int -> ?f:(int -> float) -> Camel2d_resource.label -> t
  val create_wh : x:int -> y:int -> w:int -> h:int -> ?f:(int -> float) -> Camel2d_resource.label -> t
  val render : t -> unit Renderer.t
  val update : t -> t Updater.t
  val current_frame : t -> int
end = struct
  type t = {
    x: int;
    y: int;
    size: (int * int) option;
    label: Camel2d_resource.label;
    counter: int;
    f : int -> float;
  }

  let linear_fadeout max_frame frame =
    let max_frame = float_of_int max_frame in
    let f = Float.min (float_of_int frame) max_frame in
    let f = f /. max_frame in
    1. -. f

  let linear_fadein max_frame frame =
    let max_frame = float_of_int max_frame in
    let f = Float.min (float_of_int frame) max_frame in
    let f = f /. max_frame in
    f

  let linear_fadeinout ~fadein_by ~fadeout_from ~frame_end frame =
    let frame_end = float_of_int frame_end in
    let frame = Float.min (float_of_int frame) frame_end in
    let fadein_by = float_of_int fadein_by in
    let fadeout_from = float_of_int fadeout_from in
    if frame < fadein_by then
      frame /. fadein_by
    else if frame < fadeout_from then
      1.
    else
      1. -. ((frame -. fadeout_from) /. (frame_end -. fadeout_from))


  let create ~x ~y ?(f = linear_fadein 60) label =
    {x; y; size = None; label; counter = 0; f}

  let create_wh ~x ~y ~w ~h ?(f = linear_fadein 60) label =
    {x; y; size = Some (w, h); label; counter = 0; f}

  let render t =
    let open Camel2d_renderer in
    let alpha = t.f t.counter in
    put_alpha alpha
    >> match t.size with
      | None -> Image.render_xy ~x:t.x ~y:t.y t.label
      | Some (w, h) -> Image.render_xywh ~x:t.x ~y:t.y ~w ~h t.label

  let update t =
    let open Updater in
    let* (w, h) = get_image_size t.label in
    let size = if Option.is_none t.size then Some (w, h) else t.size in
    let counter = (t.counter + 1) mod Int.max_int in
    return {t with size; counter}

  let current_frame t = t.counter
end