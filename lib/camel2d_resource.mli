
module Image = Camel2d_resource_image
module Audio = Camel2d_resource_audio

type label
type bucket
type audio_mode = SE | BGM
type 'a factory

val gen_label : unit -> label

val return : 'a -> 'a factory
val bind : 'a factory -> f:('a -> 'b factory) -> 'b factory
val (let*) : 'a factory -> ('a -> 'b factory) -> 'b factory
val (>>) : 'a factory -> 'b factory -> 'b factory

val set_image_root : string -> unit factory
val set_audio_root : string -> unit factory
val set_audio_mode : audio_mode -> unit factory
val load_image : label -> string -> unit factory
val load_audio : label -> string -> unit factory
val run : Camel2d_context.t -> unit factory -> bucket Promise.promise
val fetch_image : bucket -> label -> Image.t
val fetch_audio : bucket -> label -> Audio.t