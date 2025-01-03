open Promise

type entity = Camel2d_entity.t
type arbitrator = Camel2d_arbitrator.t
type resource_bucket = Camel2d_resource.bucket

module type T = sig
  val load_resources: unit -> resource_bucket promise 
  val start: unit -> entity list * arbitrator
end

