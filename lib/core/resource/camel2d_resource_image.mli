
type t

val load : string -> t Promise.promise
val render : ?alpha:float -> t -> Camel2d_context.t -> x:int -> y:int -> w:int -> h:int -> unit
  