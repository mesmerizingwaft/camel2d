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

val create_style :
  ?color: color ->
  ?outline: outline ->
  ?font_face: string option ->
  ?letter_spacing: string ->
  int -> text_style

val text_width_of: style:text_style -> string -> int
val create: style: text_style -> pos:(int * int) -> ?is_visible: bool -> ?base_horizontal: base_horizontal -> ?z_index: int -> ?alpha: float -> string -> string -> t