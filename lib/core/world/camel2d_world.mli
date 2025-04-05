
type state = {
    context: Camel2d_context.t;
    bucket: Camel2d_resource.bucket;
    alpha: float;
    text_style: Camel2d_text_style.t;
}
type 'a next_scene =
  | NewScene of string
  | Continue of 'a * state
type ('a, 'mode) _t = state -> 'a next_scene

val new_state : Camel2d_context.t -> Camel2d_resource.bucket -> state
val return : 'a -> ('a, _) _t
val run : state:state -> ('a, _) _t -> 'a next_scene
val bind : ('a, 'mode) _t -> ('a -> ('b, 'mode) _t) -> ('b, 'mode) _t
val map : ('a, 'mode) _t -> ('a -> 'b) -> ('b, 'mode) _t

val (>>=) : ('a, 'mode) _t -> ('a -> ('b, 'mode) _t) -> ('b, 'mode) _t
val (let*) : ('a, 'mode) _t -> ('a -> ('b, 'mode) _t) -> ('b, 'mode) _t
val (let+) : ('a, 'mode) _t -> ('a -> 'b) -> ('b, 'mode) _t
val (>>) : ('a, 'mode) _t -> ('b, 'mode) _t -> ('b, 'mode) _t

val get : (state, _) _t
val put : state -> (unit, _) _t

val set_font_size : int -> (unit, _) _t
val set_letter_spacing : string -> (unit, _) _t
val set_font_face : Camel2d_resource.label -> (unit, _) _t
val clear_text_style : (unit, _) _t

val get_canvas_size : ((int * int), _) _t
val get_text_size : string -> (int * int, _) _t
val get_image_size : Camel2d_resource.label -> (int * int, _) _t
val get_anime_size : Camel2d_resource.label -> int -> (int * int, _) _t
val print_endline : string -> (unit, _) _t
