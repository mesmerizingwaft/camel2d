open Camel2d

include module type of Preset.Basic.Image

type _t = t
type t 

val load_resources : unit Resource.factory
val create : int -> int -> t
val render : t -> unit Renderer.t
val update : t -> t Updater.t