
type 'a t = state -> 'a next_scene
and 'a next_scene =
  | NewScene of string
  | Continue of 'a * state
and state = {
  renderables: Camel2d_entity.Renderable.t list;
  bucket: Camel2d_resource.bucket
}

let new_state bucket =
  let renderables = [] in
  {bucket; renderables}

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

let ifm cond t1 t2 = let* b = cond in if b then t1 else t2
let exec_when cond t = ifm cond t (return ())

let get = fun state -> Continue (state, state)
let put state = fun _ -> Continue ((), state)

let get_bucket = let+ {bucket; _} = get in bucket
let get_renderables = let+ {renderables; _} = get in renderables

(*
let put_bucket bucket = let* state = get in put {state with bucket}
*)
let put_renderables renderables = let* state = get in put {state with renderables}

let dbg_show_renderables =
  let id_of r = Camel2d_entity.Renderable.(r.id) in
  let+ renderables = get_renderables in
  let ids = List.map id_of renderables in
  print_endline @@ Printf.sprintf "[%s]" (String.concat ";" ids)

module Condition = struct
  type r = Camel2d_entity.Renderable.t

  type t = C of (r -> bool)
    
  let con_op : (bool -> bool -> bool) -> t -> t -> t = fun op (C c1) (C c2) ->
    C (fun r -> op (c1 r) (c2 r))

  let (&&&) c1 c2 = con_op (&&) c1 c2
  let (|||) c1 c2 = con_op (||) c1 c2

  let true_ = C (fun _ -> true)
  let false_ = C (fun _ -> false)

  let lift b = if b then true_ else false_

  let any_of cs =
    C (fun r -> List.exists (fun (C c) -> c r) cs)

  let has_id id = C (fun entity ->
    Camel2d_renderable_utils.has_id id entity
  )

  let is_in x y = C (fun entity ->
    Camel2d_renderable_utils.is_in x y entity
  )

  let visible = C (fun entity ->
    Camel2d_entity.Renderable.(entity.is_visible)
  )

  let x_is_smaller_than xx = C (fun entity ->
    Camel2d_renderable_utils.x_is_smaller_than xx entity
  )

  let check_collision_with r = C (fun entity ->
    Camel2d_renderable_utils.check_collision r entity
  )

end

module Updator = struct
  type r = Camel2d_entity.Renderable.t

  type t = U : (r -> r) -> t

  let show = U Camel2d_renderable_utils.show
  let hide = U Camel2d_renderable_utils.hide
  let update_x new_x = U (Camel2d_renderable_utils.update_x new_x)
  let update_y new_y = U (Camel2d_renderable_utils.update_y new_y)
  let update_x_by_diff diff = U (Camel2d_renderable_utils.update_x_by_diff diff)
  let update_y_by_diff diff = U (Camel2d_renderable_utils.update_y_by_diff diff)
  let replace_by entity = U (fun _ -> entity)
end

let id_of (Condition.C c) =
    let* renderables = get_renderables in
    let e = List.find c renderables in
    return Camel2d_entity.Renderable.(e.id)

let update_when (Condition.C c) (Updator.U u) =
  let* renderables = get_renderables in
  let renderables = List.map (fun item -> if c item then u item else item) renderables in
  put_renderables renderables

let remove_when (Condition.C c) =
  let* renderables = get_renderables in
  let renderables = List.filter (fun item -> not @@ c item) renderables in
  put_renderables renderables

let exists (Condition.C c) =
  let+ renderables = get_renderables in
  List.exists c renderables

let spawn new_renderables =
  let* renderables = get_renderables in
  put_renderables (renderables @ new_renderables)

let replace_by_id id new_entity =
  update_when Condition.(has_id id) (Updator.replace_by new_entity)

let use_ref r =
  let* _ = return () in
  return !r

let put_ref r v =
  let* _ = return () in
  return (r := v)

let print_endline msg =
  let+ _ = return () in
  print_endline msg

let play_audio label =
  let* bucket = get_bucket in
  let audio = Camel2d_resource.fetch_audio bucket label in
  Camel2d_resource.Audio.play audio;
  return ()

let stop_audio label =
  let* bucket = get_bucket in
  let audio = Camel2d_resource.fetch_audio bucket label in 
  Camel2d_resource.Audio.stop audio;
  return ()

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

let find (Condition.C c) =
  let+ renderables = get_renderables in
  List.find c renderables

let num_of (Condition.C c) =
  let+ renderables = get_renderables in
  let renderables = List.filter c renderables in
  List.length renderables
