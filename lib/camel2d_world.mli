
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

val get_bucket: Camel2d_resource.bucket t
val get_renderables : (Camel2d_entity.Renderable.t list) t
val get_playables : (Camel2d_entity.Playable.t list) t

val put_playables : Camel2d_entity.Playable.t list -> unit t

val dbg_show_renderables : unit t
val dbg_show_playables : unit t
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
  type 'a t
  val (&&&) : 'a. 'a t -> 'a t -> 'a t
  val (|||) : 'a. 'a t -> 'a t -> 'a t
  val true_r : Camel2d_entity.Renderable.t t
  val true_p : Camel2d_entity.Playable.t t
  val false_r : Camel2d_entity.Renderable.t t
  val false_p : Camel2d_entity.Playable.t t
  val lift_r : bool -> Camel2d_entity.Renderable.t t
  val lift_p : bool -> Camel2d_entity.Playable.t t
  val any_of : 'a. 'a t list -> 'a t
  val has_id_r : string -> Camel2d_entity.Renderable.t t
  val has_id_p : string -> Camel2d_entity.Playable.t t
  val is_in : int -> int -> Camel2d_entity.Renderable.t t
  val visible : Camel2d_entity.Renderable.t t
end

module Updator: sig
  type 'a t
  val show : Camel2d_entity.Renderable.t t
  val hide : Camel2d_entity.Renderable.t t
  val replace_by_r : Camel2d_entity.Renderable.t -> Camel2d_entity_renderable.t t
  val replace_by_p : Camel2d_entity.Playable.t -> Camel2d_entity_playable.t t
  val play : Camel2d_entity.Playable.t t
  val stop : Camel2d_entity.Playable.t t
end

val id_of : 'a Condition.t -> string t
val update_when : 'a Condition.t -> 'a Updator.t -> unit t
val exists : 'a Condition.t -> bool t
val spawn_r : Camel2d_entity.Renderable.t list -> unit t
val spawn_p : Camel2d_entity.Playable.t list -> unit t
val replace_by_id_r : string -> Camel2d_entity.Renderable.t -> unit t
val replace_by_id_p : string -> Camel2d_entity.Playable.t -> unit t

val use_ref : 'a ref -> 'a t
val put_ref : 'a ref -> 'a -> unit t
val print_endline : string -> unit t

val play_bgm : string -> unit t
val stop_bgm : string -> unit t

val to_front : string -> unit t