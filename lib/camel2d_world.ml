
type 'a t = state -> 'a next_scene
and 'a next_scene =
  | NewScene of string
  | Continue of 'a * state
and state = {
  entities: Camel2d_entity.t list;
  bucket: Camel2d_resource.bucket
}

let create_state bucket entities = {bucket; entities}

let return a = fun state -> Continue (a, state)
let start_scene name = fun _ -> NewScene name
let run ~state (t: unit t) = t state
let bind (t: 'a t) ~(f: 'a -> 'b t) = fun state ->
  match t state with
    | NewScene name -> NewScene name
    | Continue (a, state) -> f a state
let map t ~f = bind t ~f:(fun a -> return (f a))

let (let*) t f = bind t ~f
let (let+) t f = map t ~f
let (>>) t1 t2 = let* _ = t1 in t2

let get = fun state -> Continue (state, state)
let put state = fun _ -> Continue ((), state)

let get_entities = let* {entities; _} = get in return entities
let get_bucket = let* {bucket; _} = get in return bucket
let get_renderables = let+ entities = get_entities in Camel2d_entity.renderables_of entities
let get_playables = let+ entities = get_entities in Camel2d_entity.playables_of entities

let put_entities entities = let* state = get in
  put {state with entities}
let put_bucket bucket = let* state = get in
  put {state with bucket}
let put_renderables renderables = let* state = get in
  let p e = Camel2d_entity.P e in
  let r e = Camel2d_entity.R e in
  let entities = Camel2d_entity.playables_of state.entities in
  let entities = (List.map p entities) @ (List.map r renderables) in
  put {state with entities}
let put_playables playables = let* state = get in
  let p e = Camel2d_entity.P e in
  let r e = Camel2d_entity.R e in
  let entities = Camel2d_entity.renderables_of state.entities in
  let entities = (List.map r entities) @ (List.map p playables) in
  put {state with entities}
