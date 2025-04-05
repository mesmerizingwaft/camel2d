include Camel2d_world

type sound_mixer
type 'a t = ('a, sound_mixer) _t

let play_sound label =
  let* {bucket; _} = get in
  let audio = Camel2d_resource.fetch_audio bucket label in
  Camel2d_resource.Audio.play audio;
  return ()

let stop_all_sounds =
  let* _ = get in
  Camel2d_resource.Audio.stop_all ();
  return ()
