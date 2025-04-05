open Camel2d

type t

type item =
    | Chamoile
    | Strawberry

val load_resources : unit Resource.factory
val create : int -> int -> t
val render : t -> unit Renderer.t
val update : Event.t -> t -> t Updater.t
val get_item : f:(int -> int -> int -> int -> bool) -> t -> (item option) * t
