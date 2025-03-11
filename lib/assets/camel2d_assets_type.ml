module type T = sig
  val load_resources : unit -> unit Camel2d_resource.factory
  val initialize: Camel2d_context.t -> unit Camel2d_world.t
  val update: Camel2d_context.t -> unit Camel2d_world.t
  val handle_event: Camel2d_context.t -> Camel2d_event.t -> unit Camel2d_world.t
end