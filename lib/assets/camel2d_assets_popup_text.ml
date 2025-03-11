open Camel2d_entity
open TextLabel

let create
  ~style
  ~pos
  ~f
  ?(n=10)
  ?(base_horizontal=BHLeft)
  id text : (module Camel2d_assets_type.T) =
(module struct
  let range = List.of_seq @@ Seq.take n (Seq.ints 0)

  let load_resources () = Camel2d_resource.return ()

  let initialize context =
    let open Camel2d_world in
    let labels = List.map (fun t ->
      let id = id ^ "_" ^ string_of_int t in
      let pt = style.pt + f t in
      let style = { style with pt } in
      let is_visible = false in
      create ~context ~style ~pos ~is_visible ~base_horizontal id text
    ) range in
    spawn labels

  let update _context = 
    let open Camel2d_world in
    let cand_ids = List.map (fun t -> id ^ "_" ^ string_of_int t) range in
    let is_cand = Condition.(any_of (List.map has_id cand_ids)) in
    let* is_not_init = exists Condition.(is_cand &&& visible) in
    if not is_not_init then begin
      let id = id ^ "_0" in
      update_when Condition.(has_id id) Updator.show
      >> to_front id
    end
    else
      let* target_id = id_of Condition.(is_cand &&& visible) in
      let target_no =
        String.split_on_char '_' target_id
        |> List.rev
        |> List.hd
        |> int_of_string
      in
      if target_no < (n - 1) then
        let next_no = target_no + 1 in
        let target_id = id ^ "_" ^ string_of_int next_no in
        let is_target = Condition.has_id target_id in
        update_when is_cand Updator.hide
        >> update_when is_target Updator.show
        >> to_front target_id
      else
        return ()

  let handle_event _context _ev = Camel2d_world.return ()

end)
