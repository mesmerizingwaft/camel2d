# camel2d

[![test-sample-availability](https://github.com/mesmerizingwaft/camel2d/actions/workflows/main.yml/badge.svg)](https://github.com/mesmerizingwaft/camel2d/actions/workflows/main.yml)

## Usage

### Overview

```OCaml
open Camel2d

let your_game = Game.create ()

module InitialScene : Scene.T = struct
    ...
end

module GameMain : Scene.T = struct
    module ResourceLabels = struct
        open Resource
        let bgm1 = gen_label ()
        let bg = gen_label ()
    end
    let load_resources =
        let open Resource in
        let open ResourceLabels in
        set_audio_root "/static/audio/"
        >> set_audio_mode BGM
        >> load_audio bgm1 "bgm1.mp3"
        >> set_image_root "/static/imgs/"
        >> load_image bg "bg.jpg"

    let initialize context =
        let sw, sh = your_game.width, your_game.height in
        let bgm1 = Entity.Playable.(
            create "bgm" ResourceLabels.bgm1 |> set_to_play
        ) in
        let bg =
            let open Entity.Renderable in
            SingleImage.create
                "bg"
                ResourceLabels.bg
                ~pos:(0,0)
                ~size:(sw, sh)
        in
        let open World in
        spawn_p [bgm1]
        >> spawn_r [bg]

    let handle_event context ev =
        let open World in
        match ev with
            | Event.MouseUp _ -> start_scene "init"
            | _ -> return ()

    let update context = ...
end

let _ =
    Game.add_scene your_game "init" (module InitialScene);
    Game.add_scene your_game "main" (module GameMain);
    start your_game "init"
```

## How to run the sample projects

```bash
./scripts/<project_name>
```

### Sample projects

| name   | description |
| ------ | ------------|
| chohan | A simple game to guess if the summation of 2 dice is odd or even. |
| popping_alfie | A simple action game that has tweet button to share the score. |
