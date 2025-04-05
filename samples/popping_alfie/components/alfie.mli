open Camel2d

type t

val load_resources : unit Resource.factory
val create : int -> t
val render : t -> unit Renderer.t
val update : Event.t -> t -> t Updater.t
val jump : int -> t -> t
val jumpable : t -> bool
val check_collision : int -> int -> int -> int -> t -> bool
