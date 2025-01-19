type 'a world = 'a Camel2d_world.t
type context = Camel2d_context.t
type bucket = Camel2d_resource.bucket
type event = Camel2d_event.t
type label = Camel2d_resource.label
type 'a factory = 'a Camel2d_resource.factory

module type T = sig
  val load_resources : unit factory
  val initialize : context -> unit world
  val update : context -> unit world
  val handle_event : context -> event -> unit world
end

let load_resources context (module Scene : T) =
  let open Promise in
  let* bucket = Camel2d_resource.run context Scene.load_resources in
  Camel2d_event.clear ();
  return bucket
