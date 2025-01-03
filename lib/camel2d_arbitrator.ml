type entity = Camel2d_entity.t
type resource_bucket = Camel2d_resource.bucket

type t = entity list -> decision
and decision =
  | Discharge of entity list * t * resource_bucket
  | Update of entity list
  | LoadScene of string

let do_nothing entities = Update entities
