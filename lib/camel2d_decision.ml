type entity = Camel2d_entity.t

type t =
  | Update of entity list
  | LoadScene of string