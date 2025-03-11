type 'a state = Pending | Resolved of 'a | Rejected of exn
type 'a promise
type 'a resolver
val make : unit -> 'a promise * 'a resolver
val return : 'a -> 'a promise
val state : 'a promise -> 'a state
val resolve : 'a resolver -> 'a -> unit
val reject : 'a resolver -> exn -> unit
val (>>=) : 'a promise -> ('a -> 'b promise) -> 'b promise
val (let*) : 'a promise -> ('a -> 'b promise) -> 'b promise
val run : 'a promise -> 'a
val join : unit promise list -> unit promise
val ignore: 'a promise -> unit promise
val is_resolved: 'a promise -> bool
val str_of_state: 'a state -> string
val continue_after_resolved: 'a promise -> ('a -> unit) -> unit

module List: sig
  include module type of List
  val sequence: 'a promise list -> 'a list promise
end