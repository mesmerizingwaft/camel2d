include module type of Camel2d_world

type renderer
type 'a t = ('a, renderer) _t

val return : 'a -> 'a t

val put_alpha : float -> unit t
val set_color : Camel2d_color.t -> unit t
val set_outline : Camel2d_text_style.outline -> unit t

module Image : sig
  val render_xy : x:int -> y:int -> Camel2d_resource.label -> unit t
  val render_xywh : x:int -> y:int -> w:int -> h:int -> Camel2d_resource.label -> unit t
end

module Anime : sig
  val render_xy : x:int -> y:int -> Camel2d_resource.label -> int -> unit t
  val render_xywh : x:int -> y:int -> w:int -> h:int -> Camel2d_resource.label -> int -> unit t
end

module Text : sig
  val draw : x:int -> y:int -> string -> unit t
  val draw_centerized : x:int -> y:int -> string -> unit t
end

module Symbol : sig
  val draw_play_button : x:int -> y:int -> radius:int -> color:Camel2d_color.t -> unit t
end