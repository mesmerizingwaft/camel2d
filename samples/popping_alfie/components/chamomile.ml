open Camel2d
open Entity.Renderable

include SingleImage

module ResourceLabels = struct
  open Resource
  let chamomile = gen_label ()
end

module Id = struct
  let chamomile = "chamomile"
end

let load_resources () =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/popping_alfie/static/imgs/"
  >> load_image chamomile "flower_chamomile.png"

let create ground_height h_max screen_w =
  let gen () =
    let open World in
    let h = ground_height - Random.int h_max in
    let pos = screen_w, h in
    let size = 30, 30 in
    let c = create Id.chamomile ResourceLabels.chamomile ~pos ~size in
    spawn [c]
  in
  let updator =
    let open World in
    remove_when Condition.(has_id Id.chamomile &&& x_is_smaller_than 0)
    >> update_when Condition.(has_id Id.chamomile) Updator.(update_x_by_diff (-3))
  in
  gen, updator