type entity = Camel2d_entity.t
type decision = Camel2d_decision.t

type t = entity list -> decision

let init entities = Camel2d_decision.Update entities
