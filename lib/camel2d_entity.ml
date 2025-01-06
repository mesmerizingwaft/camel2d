open Js_of_ocaml

type context = Camel2d_context.t
type resource_bucket = Camel2d_resource.bucket

type t = E : {
  id: string;
  render: t -> context -> resource_bucket -> unit;
  x: int;
  y: int;
  w: int;
  h: int;
} -> t

let render ((E {render; _}) as entity) = render entity

let update_when condition f = List.map (fun entity ->
  if condition entity then f entity else entity
)
let remove_when condition = List.filter (fun entity -> not (condition entity))
let count_when condition entities = List.length @@ List.filter condition entities

let (&&&) c1 c2 entity = c1 entity && c2 entity
let (|||) c1 c2 entity = c1 entity || c2 entity

let has_id target_id (E {id; _}) = target_id = id
let is_in mx my (E {x; y; w; h; _}) =
  x <= mx && mx <= x + w && y <= my && my <= y + h
let x_is_smaller_than xx (E {x; _}) = x < xx

let check_collision (E e1) (E e2) =
  let cx1, cy1 = e1.x + e1.w / 2, e1.y + e1.h / 2 in
  let cx2, cy2 = e2.x + e2.w / 2, e2.y + e2.h / 2 in
  let dx, dy = Int.abs (cx1 - cx2), Int.abs (cy1 - cy2) in
  let lw, lh = (e1.w + e2.w) / 2, (e1.h + e2.h) / 2 in
  dx <= lw && dy <= lh

let create_from_image id resource_name ~pos ~size =
  let (x, y), (w, h) = pos, size in
  let render (E {x; y; w; h; _}) context resource_bucket =
    let img = Camel2d_resource.fetch_img resource_bucket resource_name in
    let ctx = Camel2d_context.get_context2d context in
    let x, y = float_of_int x, float_of_int y in
    let w, h = float_of_int w, float_of_int h in
    let src_x, src_y, src_w, src_h = 0., 0., float_of_int img##.width, float_of_int img##.height in
    ctx##drawImage_full img src_x src_y src_w src_h x y w h
  in
  E { id; render; x; y; w; h }

module TextLabel: sig
  type color = RGBA of int * int * int * float
  type outline = NoOutline | Edging of color
  type base_horizontal = BHCenter | BHLeft
  type text_style

  val create_style :
    ?color: color ->
    ?outline: outline ->
    ?font_face: string option ->
    ?letter_spacing: string ->
    int -> text_style
  
  val text_width_of: context:Camel2d_context.t -> style:text_style -> string -> int
  val create: context: Camel2d_context.t -> style: text_style -> pos:(int * int) -> ?base_horizontal: base_horizontal -> string -> string -> t
end = struct
  type color = RGBA of int * int * int * float
  type outline = NoOutline | Edging of color
  type base_horizontal = BHCenter | BHLeft
  type text_style = {
    pt: int;
    color: color;
    outline: outline;
    font_face: string option;
    letter_spacing: string;
  }

  let create_style
    ?(color=RGBA(255, 255, 255, 1.))
    ?(outline=NoOutline)
    ?(font_face=None)
    ?(letter_spacing="normal")
    pt =
    { pt; color; outline; font_face; letter_spacing }

  let _js_str_of_color = function
    | RGBA (r, g, b, a) -> Js.string @@ Printf.sprintf "rgba(%d,%d,%d,%f)" r g b a
    
  let _pt_as_px style = Printf.sprintf "%dpx" style.pt

  let _js_font_of style =
    let pt = _pt_as_px style in
    let font_face = Option.value style.font_face ~default:"caption" in
    Js.string @@ Printf.sprintf "%s %s" pt font_face

  let _with_style style context k =
    let ctx = Camel2d_context.get_context2d context in
    ctx##save;
    ctx##.fillStyle := _js_str_of_color style.color;
    ctx##.font := _js_font_of style;
    ctx##.textBaseline := Js.string "top";
    Ext.set_letter_spacing_of ctx style.letter_spacing;
    let result = k ctx in
    ctx##restore;
    result

  let _render_outline ctx outline text x y =
    (match outline with
    | NoOutline -> ()
    | Edging color ->
      ctx##save;
      ctx##.lineWidth := 3.;
      ctx##.fillStyle := _js_str_of_color color;
      ctx##strokeText (Js.string text) (float_of_int x) (float_of_int y);
      ctx##restore;
    )

  let text_width_of ~context ~style text =
    let text = Js.string text in
    _with_style style context (fun ctx -> (ctx##measureText text)##.width)
    |> int_of_float

  let create ~context ~style ~pos ?(base_horizontal=BHLeft) id text =
    let (x, y) = pos in
    let (w, h) = text_width_of ~context ~style text, style.pt in
    let render (E {x; y; _}) context _ =
      _with_style style context (fun ctx ->
        let x = if base_horizontal = BHCenter
          then x - w / 2
          else x
        in
        _render_outline ctx style.outline text x y;
        ctx##fillText (Js.string text) (float_of_int x) (float_of_int y);
        ctx##restore
      )
    in
    E { id; render; x; y; w; h }
end

let create_from_text
  ?(color=`RGBA(255, 255, 255, 1.))
  ?(outline=`NoOutline)
  ?(font_face=None)
  ?(letter_spacing="normal")
  ?(base_horizontal=`Left)
  ~pt ~pos ~size id text =
  let (x, y), (w, h) = pos, size in
  let render (E {x; y; _}) context _ =
    let ctx = Camel2d_context.get_context2d context in
    ctx##save;
    ctx##.fillStyle := begin
      match color with
        | `RGBA (r,g,b,a) ->
          Js.string @@ Printf.sprintf "rgba(%d,%d,%d,%f)" r g b a
        | _ -> failwith "invalid color"
    end;
    let pt = Printf.sprintf "%dpx" pt in
    let font_face = Option.value font_face ~default:"caption" in
    ctx##.font := Js.string @@ Printf.sprintf "%s %s" pt font_face;
    let x = if base_horizontal = `Center
      then x - (int_of_float ((ctx##measureText (Js.string text))##.width /. 2.))
      else x
    in
    ctx##.textBaseline := Js.string "top";
    Ext.set_letter_spacing_of ctx letter_spacing;
    (match outline with
    | `NoOutline -> ()
    | `Edging (r,g,b) ->
      ctx##save;
      ctx##.lineWidth := 3.;
      ctx##.fillStyle := Js.string @@ Printf.sprintf "rgb(%d,%d,%d)" r g b;
      ctx##strokeText (Js.string text) (float_of_int x) (float_of_int y);
      ctx##restore;
    );
    ctx##fillText (Js.string text) (float_of_int x) (float_of_int y);
    ctx##restore;
  in
  E { id; render; x; y; w; h }