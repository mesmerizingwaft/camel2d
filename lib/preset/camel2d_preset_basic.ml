module Color = Camel2d_color
module Updater = Camel2d_updater
module Renderer = Camel2d_renderer
type context = Camel2d_context.t
type color = Camel2d_color.t

module Image : sig
  type t
  val update : Camel2d_event.t -> t -> t Updater.t
  val render : t -> unit Renderer.t
  val create : x:int -> y:int -> Camel2d_resource.label -> t
  val create_wh : x:int -> y:int -> w:int -> h:int -> Camel2d_resource.label -> t
  val update_x : int -> t -> t
  val update_y : int -> t -> t
  val update_w : int -> t -> t
  val update_h : int -> t -> t
  val update_label : Camel2d_resource.label -> t -> t
  val x_of : t -> int
  val y_of : t -> int
  val w_of : t -> int
  val h_of : t -> int
  val clicked : t -> bool
  val unclick : t -> t
  val check_inclusion : t -> int -> int -> bool Updater.t
end = struct
  type t = {
    x: int;
    y: int;
    size: (int * int) option;
    label: Camel2d_resource.label;
    is_clicked: bool;
  }

  let render t =
    let open Camel2d_renderer in
    match t.size with
      | None -> Image.render_xy ~x:t.x ~y:t.y t.label
      | Some (w, h) -> Image.render_xywh ~x:t.x ~y:t.y ~w ~h t.label

  let create ~x ~y label =
    {x; y; size = None; label; is_clicked = false}
  let create_wh ~x ~y ~w ~h label =
    {x; y; size = Some (w, h); label; is_clicked = false}

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
  let update_label label t = {t with label}
  let x_of t = t.x
  let y_of t = t.y
  let w_of t = match t.size with
    | None -> failwith "To get width, you need to call update first"
    | Some (w, _) -> w
  let h_of t = match t.size with
    | None -> failwith "To get height, you need to call update first"
    | Some (_, h) -> h

  let clicked t = t.is_clicked
  let unclick t = {t with is_clicked = false}

  let check_inclusion t x y =
    let open Updater in
    match t.size with
      | None -> failwith "To check inclusion, you need to call update first"
      | Some (w, h) ->
        let tx = t.x in
        let ty = t.y in
        return (x >= tx && x <= tx + w && y >= ty && y <= ty + h)

  let update e t =
    let open Updater in
    match e with
      | Camel2d_event.Tick ->
        let* (w, h) = get_image_size t.label in
        let size = if Option.is_none t.size then Some (w, h) else t.size in
        return {t with size}
      | MouseUp {x; y; _} ->
        let* included = check_inclusion t x y in
        return {t with is_clicked = included}
      | _ -> return t

end


module Text : sig
  module Style : sig
    include module type of Camel2d_preset_textstyle    
  end

  type style = Camel2d_preset_textstyle.t

  type t
  
  val render : t -> unit Renderer.t
  val update : Camel2d_event.t -> t -> t Updater.t
  val create: style: style -> x:int -> y:int -> string -> t
  val create_centerized: style: style -> x:int -> y:int -> string -> t
  val clicked : t -> bool
  val update_font_size : int -> t -> t
  val update_text : string -> t -> t
  val unclick : t -> t
end = struct
  module Style = Camel2d_preset_textstyle
  type style = Style.t

  type t = {
    x: int;
    y: int;
    text: string;
    style: style;
    centerized: bool;
    is_clicked: bool;
    is_mouseon: bool;
  }
  let render t =
    let open Renderer in
    let color = match t.is_mouseon, t.style.color_mouse_on with
      | true, Some color -> color
      | _ -> t.style.color
    in
    clear_text_style
    >> set_font_size t.style.pt
    >> set_color color
    >> set_outline t.style.outline
    >> set_letter_spacing t.style.letter_spacing
    >> Option.value (
      Option.map (fun label -> set_font_face label) t.style.font_face
    ) ~default:(return ())
    >> if t.centerized then
      Text.draw_centerized ~x:t.x ~y:t.y t.text
    else
      Text.draw ~x:t.x ~y:t.y t.text

  let _get_text_size t =
    let open Updater in
    (match t.style.font_face with
      | None -> return ()
      | Some font -> set_font_face font)
    >> set_font_size t.style.pt
    >> set_letter_spacing t.style.letter_spacing
    >> get_text_size t.text

  let check_inclusion t x y =
    let open Updater in
    let* tw, th = _get_text_size t in
    let tx = if t.centerized then t.x - tw / 2 else t.x in
    let ty = if t.centerized then t.y - th / 2 else t.y in
    let open Camel2d_preset_collision_detection.Rectangle in
    return (check_inclusion ~x:tx ~y:ty ~w:tw ~h:th x y)

  let update e t =
    let open Updater in
    match e with
      | Camel2d_event.MouseMove {x; y} ->
        let* included = check_inclusion t x y in
        return {t with is_mouseon = included}
      | MouseUp {x; y; _} ->
        let* included = check_inclusion t x y in
        return {t with is_clicked = included}
      | _ -> return t

  let create ~style ~x ~y text = {
    x; y; text; style; centerized = false; is_clicked = false; is_mouseon = false
  }
  let create_centerized ~style ~x ~y text = {
    x; y; text; style; centerized = true; is_clicked = false; is_mouseon = false
  }
  let clicked t = t.is_clicked
  let update_text text t = {t with text}
  let update_font_size pt t = {t with style = Style.set_font_size pt t.style}
  let unclick t = {t with is_clicked = false}
end