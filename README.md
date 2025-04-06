# camel2d

[![build](https://github.com/mesmerizingwaft/camel2d/actions/workflows/main.yml/badge.svg)](https://github.com/mesmerizingwaft/camel2d/actions/workflows/main.yml)

## Usage

### Overview

Here's an overview of the structure of a game created with this library. Please see [samples](https://github.com/mesmerizingwaft/camel2d/tree/master/samples) for more concrete examples.

```OCaml
open Camel2d

let _ = Random.self_init ()
let literals = Literals.t

module AudioAgreement = Preset.Scene.AudioAgreement.Make(struct let next_scene = "main" end)

module GameMain : Scene.T = struct
  module ResourceLabels = struct
    open Resource
    let font_tamanegi = gen_label ()
    (* ... *)
  end

  type t = unit (* game state *)

  let load_resources =
    let open Resource in
    let open ResourceLabels in
    set_font_root "static/fonts/"
    >> load_font font_tamanegi "tamanegi_v7.ttf"

  let init _context = ()
  let renderer t = Renderer.return ()
  let sound_mixer t = SoundMixer.return ()
  let updater _e t = Updater.return t

end

let _ = 
  let game = Game.create () in
  Game.add_scene game "audio_agreement" (module AudioAgreement);
  Game.add_scene game "main" (module GameMain);
  start game "audio_agreement"
```

### Sample projects

| project_name   | description |
| -------------- | ----------- |
| chohan | A simple game to guess if the summation of 2 dice is odd or even. |
| popping_alfie | A simple action game that has tweet button to share the score. |
| novelgame | A minimum novelgame example |

* sample projects can be run by the following script:

```bash
./scripts/run <project_name>
```
