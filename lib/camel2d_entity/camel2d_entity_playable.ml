type bucket = Camel2d_resource.bucket

type state =
  | Pausing
  | Playing
  | ToPlay

type t = {
  id: string;
  update: bucket -> t -> t;
  state: state
}

let create id resource_name =
  let state = Pausing in
  let update bucket p =
    match p.state with
      | ToPlay ->
        let audio = Camel2d_resource.fetch_audio bucket resource_name in
        Camel2d_resource.Audio.play audio;
        {p with state = Playing}
      | Playing when 
        let audio = Camel2d_resource.fetch_audio bucket resource_name in
        Camel2d_resource.Audio.is_ended audio ->
        {p with state = Pausing}
      | _ -> p
  in
  {id; update; state}

let update bucket p = p.update bucket p

let set_to_play p = {p with state=ToPlay}