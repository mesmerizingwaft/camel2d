
let linear_fadeout max_frame i =
  let i = float_of_int i in
  let max_frame = float_of_int max_frame in
  max 0. (1.0 -. (1.0 /. max_frame) *. i)

let linear_fadeinout ~frame_in ~frame_out max_frame i =
  let i = float_of_int i in
  let max_frame = float_of_int max_frame in
  let frame_in = float_of_int frame_in in
  let frame_out = float_of_int frame_out in
  if i < frame_in then
    i /. frame_in
  else if i < frame_out then
    1.0
  else
    1.0 -. ((i -. frame_out) /. (max_frame -. frame_out))

let create
  ?(f_curve=linear_fadeout)
  ?(max_frame=60)
  id
  (entity: Camel2d_entity.t) =
  let range = List.of_seq @@ Seq.take max_frame (Seq.ints 0) in
  let entities = 
    List.map (fun i ->
      let alpha = f_curve max_frame i in
      let id = id ^ "_" ^ string_of_int i in
      let open Camel2d_entity in
      entity
      |> update_id id
      |> update_alpha alpha
    ) range
    |> Array.of_list
  in
  Camel2d_assets_anime.create entities ~is_loop:false

  
