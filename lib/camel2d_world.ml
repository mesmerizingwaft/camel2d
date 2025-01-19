
type 'a t = state -> 'a next_scene
and 'a next_scene =
  | NewScene of string
  | Continue of 'a * state
and state = {
  renderables: Camel2d_entity.Renderable.t list;
  playables: Camel2d_entity.Playable.t list;
  bucket: Camel2d_resource.bucket
}

let new_state bucket =
  let renderables = [] in
  let playables = [] in
  {bucket; renderables; playables}

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

let get_bucket = let+ {bucket; _} = get in bucket
let get_renderables = let+ {renderables; _} = get in renderables
let get_playables = let+ {playables; _} = get in playables

(*
let put_bucket bucket = let* state = get in put {state with bucket}
*)
let put_renderables renderables = let* state = get in put {state with renderables}
let put_playables playables = let* state = get in put {state with playables}

let dbg_show_renderables =
  let id_of r = Camel2d_entity.Renderable.(r.id) in
  let+ renderables = get_renderables in
  let ids = List.map id_of renderables in
  print_endline @@ Printf.sprintf "[%s]" (String.concat ";" ids)

let dbg_show_playables =
  let+ playables = get_playables in
  let ps = List.map Camel2d_entity.Playable.str_of playables in
  print_endline @@ Printf.sprintf "[%s]" (String.concat ";" ps)

module Condition = struct
  type r = Camel2d_entity.Renderable.t
  type p = Camel2d_entity.Playable.t

  type _ t =
    | Renderable : (r -> bool) -> r t
    | Playable : (p -> bool) -> p t

    
  let con_op : type a. (bool -> bool -> bool) -> a t -> a t -> a t = fun op c1 c2 ->
    let aux c1 c2 entity = op (c1 entity) (c2 entity) in
    match c1, c2 with
      | Renderable c1', Renderable c2' -> Renderable (aux c1' c2')
      | Playable c1', Playable c2' -> Playable (aux c1' c2')

  let (&&&) c1 c2 = con_op (&&) c1 c2
  let (|||) c1 c2 = con_op (||) c1 c2

  let true_r = Renderable (fun _ -> true)
  let true_p = Playable (fun _ -> true)
  let false_r = Renderable (fun _ -> false)
  let false_p = Playable (fun _ -> false)

  let lift_r b = if b then true_r else false_r
  let lift_p b = if b then true_p else false_p

  let any_of_ bottom cs =
    let rec inner = function
      | [] -> bottom
      | c::cs -> c ||| inner cs
    in inner cs    
  let any_of_r cs = any_of_ false_r cs
  let any_of_p cs = any_of_ false_p cs
  let any_of : type a. a t list -> a t = fun cs ->
    match cs with
      | [] -> assert false
      | (Renderable _)::_ -> any_of_r cs
      | (Playable _)::_ -> any_of_p cs

  let has_id_r id = Renderable (fun entity ->
    Camel2d_renderable_utils.has_id id entity
  )

  let has_id_p id = Playable (fun entity ->
    entity.id == id
  )

  let is_in x y = Renderable (fun entity ->
    Camel2d_renderable_utils.is_in x y entity
  )

  let visible = Renderable (fun entity ->
    Camel2d_entity.Renderable.(entity.is_visible)
  )
end

module Updator = struct
  type r = Camel2d_entity.Renderable.t
  type p = Camel2d_entity.Playable.t

  type _ t =
    | Renderable : (r -> r) -> r t
    | Playable : (p -> p) -> p t

  let show = Renderable Camel2d_renderable_utils.show
  let hide = Renderable Camel2d_renderable_utils.hide
  let replace_by_r entity = Renderable (fun _ -> entity)
  let replace_by_p entity = Playable (fun _ -> entity)
  let play = Playable Camel2d_entity.Playable.set_to_play
  let stop = Playable Camel2d_entity.Playable.set_to_stop
end

let id_of: type a. a Condition.t -> string t = function condition ->
  match condition with
    | Renderable c ->
      let* renderables = get_renderables in
      let e = List.find c renderables in
      return e.id
    | Playable c ->
      let* playables = get_playables in
      let e = List.find c playables in
      return e.id

let update_when: type a. a Condition.t -> a Updator.t -> unit t = fun condition updator ->
  let aux c f items = List.map (fun item -> if c item then f item else item) items in
  match condition, updator with
    | Renderable c, Renderable f ->
      let* renderables = get_renderables in
      aux c f renderables |> put_renderables
    | Playable c, Playable f ->
      let* playables = get_playables in
      aux c f playables |> put_playables

let exists: type a. a Condition.t -> bool t = fun condition ->
  match condition with
    | Renderable c -> let+ renderables = get_renderables in
      List.exists c renderables
    | Playable c -> let+ playables = get_playables in
      List.exists c playables

let spawn_r new_renderables =
  let* renderables = get_renderables in
  put_renderables (renderables @ new_renderables)

let spawn_p new_playables =
  let* playables = get_playables in
  put_playables (playables @ new_playables)

let replace_by_id_r id new_entity =
  update_when Condition.(has_id_r id) (Updator.replace_by_r new_entity)

let replace_by_id_p id new_entity =
  update_when Condition.(has_id_p id) (Updator.replace_by_p new_entity)

let use_ref r =
  let* _ = return () in
  return !r

let put_ref r v =
  let* _ = return () in
  return (r := v)

let print_endline msg =
  let+ _ = return () in
  print_endline msg

let play_bgm id =
  update_when Condition.(has_id_p id) Updator.play

let stop_bgm id =
  update_when Condition.(has_id_p id) Updator.stop

let to_front id =
  let* renderables = get_renderables in
  let rec inner target = function
    | [] when Option.is_none target -> raise Not_found
    | [] -> [Option.get target]
    | item::items when Camel2d_entity.Renderable.(item.id) = id ->
      inner (Some item) items
    | item::items ->
      item::(inner target items)
  in
  put_renderables @@ inner None renderables