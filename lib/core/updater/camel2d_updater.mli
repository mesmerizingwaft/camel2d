include module type of Camel2d_world

type updater
type 'a t = ('a, updater) _t

val return : 'a -> 'a t
val start_scene : string -> _ t
val resume_audio : unit t