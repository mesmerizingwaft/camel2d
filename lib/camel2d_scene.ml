open Promise

type entity = Camel2d_entity.t
type arbitrator = Camel2d_arbitrator.t
type event_handler = Camel2d_eventhandler.t
type resource_bucket = Camel2d_resource.bucket
type context = Camel2d_context.t

module type T = sig
  val load_resources: unit -> resource_bucket promise 
  val start: context -> entity list * arbitrator * event_handler
end

