include Camel2d_world

type updater
type 'a t = ('a, updater) _t


let start_scene name =
  fun _ -> Camel2d_world.NewScene name

let resume_audio =
  let* {context; _} = get in
  Camel2d_resource.Audio.resume context;
  return ()
