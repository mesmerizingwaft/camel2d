
type 'a t
type 'a next_scene =
  | NewScene of string
  | Continue of 'a * state
and state = {
    entities: Camel2d_entity.t list;
    bucket: Camel2d_resource.bucket
}

val create_state : Camel2d_resource.bucket -> Camel2d_entity.t list -> state

val return : 'a -> 'a t
val start_scene : string -> 'a t
val run : state:state -> unit t -> unit next_scene
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val map : 'a t -> f:('a -> 'b) -> 'b t

val (let*) : 'a t -> ('a -> 'b t) -> 'b t
val (let+) : 'a t -> ('a -> 'b) -> 'b t
val (>>) : 'a t -> 'b t -> 'b t

val get : state t
val put : state -> unit t

val get_entities: (Camel2d_entity.t list) t
val get_bucket: Camel2d_resource.bucket t
val get_renderables : (Camel2d_renderable.t list) t
val get_playables : (Camel2d_playable.t list) t

val put_entities: Camel2d_entity.t list -> unit t
val put_bucket: Camel2d_resource.bucket -> unit t
val put_renderables : Camel2d_renderable.t list -> unit t
val put_playables : Camel2d_playable.t list -> unit t