(** Camel2d *)

module Game = Camel2d_game
module Context = Camel2d_context
module Scene = Camel2d_scene
module World = Camel2d_world
module Event = Camel2d_event
module Resource = Camel2d_resource
module Entity = Camel2d_entity
module RenderableUtils = Camel2d_renderable_utils
module Templates = Camel2d_template
module SnsUtils = Camel2d_snsutils

val enable_audio : Camel2d_context.t -> unit
(** The entry point of the game you made *)
val start : Game.t -> string -> unit