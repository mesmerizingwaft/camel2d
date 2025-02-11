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
    let load_resources = ...
    let initialize context = ...
    let handle_event context ev = ...
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
