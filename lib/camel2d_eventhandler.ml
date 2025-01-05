type entity = Camel2d_entity.t
type decision = Camel2d_decision.t
type event = Camel2d_event.t

type t = event -> entity list -> decision

let init _ entities = Camel2d_decision.Update entities
