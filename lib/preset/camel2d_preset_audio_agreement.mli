type t

val init    : t
val render  : t -> unit Camel2d_renderer.t
val update  : t -> t Camel2d_updater.t
val handle_event : Camel2d_event.t -> t -> t Camel2d_updater.t
val clicked : t -> bool