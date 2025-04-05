module type T = sig
  type t
  val load_resources : unit Camel2d_resource.factory
  val init : Camel2d_context.t -> t
  val renderer : t -> unit Camel2d_renderer.t
  val sound_mixer : t -> t Camel2d_sound_mixer.t
  val updater : Camel2d_event.t -> t -> t Camel2d_updater.t
end

let load_resources context (module Scene : T) =
  let open Promise in
  let* bucket = Camel2d_resource.run context Scene.load_resources in
  Camel2d_event.clear ();
  return bucket
