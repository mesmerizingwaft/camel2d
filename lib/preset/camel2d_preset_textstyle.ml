
module Color = Camel2d_color
type label = Camel2d_resource.label
type outline = Camel2d_text_style.outline
type color = Camel2d_color.t

type t = {
  pt: int;
  color: color;
  color_mouse_on: color option;
  outline: outline;
  font_face: label option;
  letter_spacing: string;
}
let create () = {
  pt = 12;
  color = Color.RGB (120, 120, 120);
  color_mouse_on = None;
  outline = NoOutline;
  font_face = None;
  letter_spacing = "0";
}
let set_fontface label style =
  {style with font_face = Some label}
let set_font_size pt style =
  {style with pt}
let set_color color style =
  {style with color}
let set_color_mouse_on color style =
  {style with color_mouse_on = Some color}
let set_outline outline style =
  {style with outline}
