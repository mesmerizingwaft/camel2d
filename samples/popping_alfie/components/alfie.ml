open Camel2d
open Entity.Renderable

include SingleImage

module ResourceLabels = struct
  open Resource
  let alfie_steady = gen_label ()
  let alfie_in_air = gen_label ()
end

module Id = struct
  let alfie_steady = "alfie_steady"
  let alfie_in_air = "alfie_in_air"
end

let load_resources () =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/popping_alfie/static/imgs/"
  >> load_image alfie_steady "alfie1.png"
  >> load_image alfie_in_air "alfie2.png"

let create ground_height =
  let h = ref 0 in
  let init _context =
    let pos = 0, 0 in
    let alfie_steady =
      let size = 80, 50 in
      create ~pos ~size ~is_visible:true Id.alfie_steady ResourceLabels.alfie_steady
    in
    let alfie_in_air =
      let size = 90, 50 in
      create ~pos ~size ~is_visible:false Id.alfie_in_air ResourceLabels.alfie_in_air
    in
    let open World in
    spawn_r [alfie_steady; alfie_in_air]
  in
  let updator =
    let open World in
    let* h = use_ref h in
    update_when Condition.(has_id_r Id.alfie_steady ||| has_id_r Id.alfie_in_air) Updator.(update_y (ground_height - h))
    >> let for_alfie_steady = update_when Condition.(has_id_r Id.alfie_steady) in
    let for_alfie_in_air = update_when Condition.(has_id_r Id.alfie_in_air) in
    if h > 0 then begin
      for_alfie_steady Updator.hide
      >> for_alfie_in_air Updator.show
    end else begin
      for_alfie_steady Updator.show
      >> for_alfie_in_air Updator.hide
    end
  in
  init, updator, h