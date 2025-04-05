type t

val load : string -> t Promise.promise
val render : ?alpha:float -> context:Camel2d_context.t -> x:int -> y:int -> ?w:(int option) -> ?h:(int option) -> t -> int -> unit
val width_of : t -> int -> int
val height_of : t -> int -> int