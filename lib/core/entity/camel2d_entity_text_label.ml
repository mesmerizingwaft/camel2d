open Js_of_ocaml
open Camel2d_entity_types

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

let text_width_of ~style text =
  let context = Camel2d_context.dummy_context () in
  let text = Js.string text in
  _with_style style context (fun ctx -> (ctx##measureText text)##.width)
  |> int_of_float

let create ~style ~pos ?(is_visible=true) ?(base_horizontal=BHLeft) ?(z_index=0) ?(alpha=1.0) id text =
  let (x, y) = pos in
  (* ToDo: FONT needs to be async loaded *)
  let (w, h) = text_width_of ~style text, style.pt in
  let render {x; y; is_visible; alpha; _} context _ =
    if is_visible then begin
      _with_style style context (fun ctx ->
        ctx##.globalAlpha := alpha;
        _render_outline ctx style.outline text x y;
        ctx##fillText (Js.string text) (float_of_int x) (float_of_int y);
        ctx##restore
      )
    end
  in
  match base_horizontal with
    | BHCenter ->
      let x = x - w / 2 in
      { id; render; is_visible; x; y; w; h; z_index; alpha }
    | BHLeft -> { id; render; is_visible; x; y; w; h; z_index; alpha }
