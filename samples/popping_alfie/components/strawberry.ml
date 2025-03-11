open Camel2d
open Entity

include SingleImage

module ResourceLabels = struct
  open Resource
  let strawberry = gen_label ()
end

module Id = struct
  let strawberry = "strawberry"
end

let load_resources () =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/popping_alfie/static/imgs/"
  >> load_image strawberry "fruit_strawberry.png"

let create ground_height h_max screen_w =
  let gen () =
    let open World in
    let h = ground_height - Random.int h_max in
    let pos = screen_w, h in
    let size = 30, 30 in
    let c = create Id.strawberry ResourceLabels.strawberry ~pos ~size in
    spawn [c]
  in
  let updator =
    let open World in
    remove_when Condition.(has_id Id.strawberry &&& x_is_smaller_than 0)
    >> update_when Condition.(has_id Id.strawberry) Updator.(update_x_by_diff (-5))
  in
  gen, updator