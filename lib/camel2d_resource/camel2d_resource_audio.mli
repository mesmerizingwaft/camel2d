type t

val resume : Camel2d_context.t -> unit
val load : context: Camel2d_context.t -> ?is_loop:bool -> string -> t Promise.promise
val play : t -> unit
val stop : t -> unit
val stop_all : unit -> unit
val is_ended : t -> bool

