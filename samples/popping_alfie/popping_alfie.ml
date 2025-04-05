open Camel2d

let _ = Random.self_init ()

module AudioAgreement = Preset.Scene.SceneAudioAgreement.Make(struct let next_scene = "title" end)

let _ =
  let popping_alfie = Game.create () in
  Game.add_scene popping_alfie "audio_agreement" (module AudioAgreement);
  Game.add_scene popping_alfie "title" (module Gametitle);
  Game.add_scene popping_alfie "main" (module Gamemain);
  Game.add_scene popping_alfie "gameover" (module Gameover);
  start popping_alfie "audio_agreement"
