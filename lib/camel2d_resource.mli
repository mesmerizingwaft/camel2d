type t
type label
type bucket

type context = Camel2d_context.t

val create_bucket : unit -> bucket
val create_label : string -> label
val load : bucket:bucket -> label:label -> t -> unit Promise.promise

module Image : sig
  val load : label -> t Promise.promise
  val render : bucket -> label -> context -> x:int -> y:int -> w:int -> h:int -> unit
end

module Audio : sig
  val resume : context -> unit
  val load : context: context -> ?is_loop:bool -> label -> t Promise.promise
  val play : bucket -> label -> unit
  val stop : bucket -> label -> unit
  val stop_all : unit -> unit
end
