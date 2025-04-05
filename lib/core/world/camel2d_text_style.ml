open Js_of_ocaml

module Resource = Camel2d_resource

type outline =
| NoOutline
| Edging of Camel2d_color.t

type t = {
  pt: int;
  color: Camel2d_color.t;
  outline: outline;
  font_face: string option;
  letter_spacing: string;
}

let init = {
  pt = 16;
  color = RGB (0, 0, 0);
  outline = NoOutline;
  font_face = None;
  letter_spacing = "0";
}

let js_font_of style =
  let pt = Printf.sprintf "%dpx" style.pt in
  let font_face = Option.value style.font_face ~default:"caption" in
  Js.string @@ Printf.sprintf "%s %s" pt font_face

let with_style style alpha context k =
  let ctx = Camel2d_context.get_context2d context in
  ctx##save;
  ctx##.globalAlpha := alpha;
  ctx##.fillStyle := Camel2d_color.js_str_of style.color;
  ctx##.font := js_font_of style;
  ctx##.textBaseline := Js.string "top";
  Ext.set_letter_spacing_of ctx style.letter_spacing;
  let result = k ctx in
  ctx##restore;
  result