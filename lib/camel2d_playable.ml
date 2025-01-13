type bucket = Camel2d_resource.bucket

type state =
  | Pausing
  | Playing
  | ToPlay

type t = Playable : {
  id: string;
  update: bucket -> t -> t;
  state: state
} -> t

let create id resource_name =
  let state = Pausing in
  let update bucket (Playable p) =
    match p.state with
      | ToPlay ->
        Camel2d_resource.Audio.play bucket resource_name;
        Playable {p with state = Playing}
      | _ -> Playable p
  in
  Playable {id; update; state}

let update bucket (Playable p) =
  p.update bucket (Playable p)

let set_to_play (Playable p) = Playable {p with state=ToPlay}