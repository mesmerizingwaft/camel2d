include module type of Camel2d_world

type sound_mixer
type 'a t = ('a, sound_mixer) _t

val return : 'a -> 'a t
val play_sound : Camel2d_resource.label -> unit t
val stop_all_sounds : unit t