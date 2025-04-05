
type t

val load : string -> t Promise.promise
val render : ?alpha:float -> context:Camel2d_context.t -> x:int -> y:int -> ?w:(int option) -> ?h:(int option) -> t -> unit
val width_of : t -> int
val height_of : t -> int