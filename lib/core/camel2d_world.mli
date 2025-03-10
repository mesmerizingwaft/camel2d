
type 'a t
type 'a next_scene =
  | NewScene of string
  | Continue of 'a * state
and state

val new_state : Camel2d_resource.bucket -> state

val return : 'a -> 'a t
val start_scene : string -> 'a t
val run : state:state -> unit t -> unit next_scene
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val map : 'a t -> f:('a -> 'b) -> 'b t

val (let*) : 'a t -> ('a -> 'b t) -> 'b t
val (let+) : 'a t -> ('a -> 'b) -> 'b t
val (>>) : 'a t -> 'b t -> 'b t

val ifm : bool t -> 'a t -> 'a t -> 'a t
val exec_when : bool t -> unit t -> unit t

val get_bucket: Camel2d_resource.bucket t
val get_renderables : (Camel2d_entity.Renderable.t list) t

val dbg_show_renderables : unit t
(*

val get : state t
val put : state -> unit t
*)
(*
val get_entities: (Camel2d_entity.t list) t



val put_entities: Camel2d_entity.t list -> unit t
val put_bucket: Camel2d_resource.bucket -> unit t
val put_renderables : Camel2d_renderable.t list -> unit t

*)

module Condition: sig
  type t
  val (&&&) : 'a. t -> t -> t
  val (|||) : 'a. t -> t -> t
  val true_ : t
  val false_ : t
  val lift : bool -> t
  val any_of : t list -> t
  val has_id : string -> t
  val is_in : int -> int -> t
  val visible : t
  val x_is_smaller_than : int -> t
  val check_collision_with : Camel2d_entity.Renderable.t -> t
end

module Updator: sig
  type t
  val show : t
  val hide : t
  val update_x : int -> t
  val update_y : int -> t
  val update_x_by_diff : int -> t
  val update_y_by_diff : int -> t
  val replace_by : Camel2d_entity.Renderable.t -> t
end

val id_of : Condition.t -> string t
val update_when : Condition.t -> Updator.t -> unit t
val remove_when : Condition.t -> unit t
val exists : Condition.t -> bool t
val num_of : Condition.t -> int t
val spawn : Camel2d_entity.Renderable.t list -> unit t
val replace_by_id : string -> Camel2d_entity.Renderable.t -> unit t

val use_ref : 'a ref -> 'a t
val put_ref : 'a ref -> 'a -> unit t
val print_endline : string -> unit t

val play_audio : Camel2d_resource.label -> unit t
val stop_audio : Camel2d_resource.label -> unit t

val to_front : string -> unit t
val find : Condition.t -> Camel2d_entity.Renderable.t t