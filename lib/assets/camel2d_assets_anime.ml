
let create
  ?(n_frame=0)
  ?(is_loop=true)
  (entities : Camel2d_entity.t array)
  : (module Camel2d_assets_type.T) =
(module struct
  let frame_cnt = ref 0
  let index = ref 0
  let load_resources () = Camel2d_resource.return ()
  let initialize _context =
    let open Camel2d_world in
    put_ref frame_cnt 0
    >> put_ref index 0
    >> let* index' = use_ref index in
    spawn [entities.(index')]

  let update _context =
    let open Camel2d_world in
    let* index' = use_ref index in
    let* frame_cnt' = use_ref frame_cnt in
    if frame_cnt' < n_frame then begin
      put_ref frame_cnt (frame_cnt' + 1)
    end else begin
      let old_id = Camel2d_entity.id_of entities.(index') in
      let next_index = if is_loop
        then (index' + 1) mod (Array.length entities - 1)
        else min (index' + 1) (Array.length entities - 1)
      in
      remove_when Condition.(has_id old_id)
      >> spawn [entities.(next_index)]
      >> put_ref index next_index
      >> put_ref frame_cnt 0
    end

  let handle_event _context _ev = Camel2d_world.return ()

end)