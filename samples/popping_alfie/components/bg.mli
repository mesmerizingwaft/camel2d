open Camel2d

type t

val load_resources : unit Resource.factory
val create : int -> int -> t
val render : t -> unit Renderer.t
val update : Event.t -> t -> t Updater.t