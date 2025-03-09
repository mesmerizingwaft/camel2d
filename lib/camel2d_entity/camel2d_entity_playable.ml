type bucket = Camel2d_resource.bucket

type state =
  | Pausing
  | Playing
  | ToPlay
  | ToStop

let str_of_state = function
  | Pausing -> "Pausing"
  | Playing -> "Playing"
  | ToPlay -> "ToPlay"
  | ToStop -> "ToStop"

type t = {
  id: string;
  update: bucket -> t -> t;
  state: state
}

let str_of t =
  let state = str_of_state t.state in
  Printf.sprintf "{id=%s; state=%s}" t.id state

let create id resource_name =
  let state = Pausing in
  let update bucket p =
    let open Camel2d_resource in
    match p.state with
      | ToPlay ->
        let audio = fetch_audio bucket resource_name in
        Audio.stop audio;
        Camel2d_resource.Audio.play audio;
        {p with state = Playing}
      | Playing ->
        let audio = fetch_audio bucket resource_name in
        if Audio.is_ended audio then begin
          Audio.stop audio;
          {p with state = Pausing}
        end else p
      | ToStop ->
        let audio = Camel2d_resource.fetch_audio bucket resource_name in
        Camel2d_resource.Audio.stop audio;
        {p with state = Pausing}
      | _ -> p
  in
  {id; update; state}

let update bucket p = p.update bucket p
let set_to_play p = {p with state=ToPlay}
let set_to_stop p = {p with state=ToStop}