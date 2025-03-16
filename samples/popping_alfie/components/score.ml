open Camel2d
open Entity

include TextLabel

module Id = struct
  let label_score = "label_score"
end

let score = ref 0

let _gen_label _context =
  let style =
    let pt = 20 in
    let font_face = Some "Mochiy Pop One" in
    let color = TextLabel.RGBA (255, 255, 255, 1.) in
    create_style ~font_face ~color pt
  in
  let label_text () =
    Printf.sprintf "SCORE: %d" !score
  in
  create ~style ~pos:(10, 10) Id.label_score (label_text ())  

let create () =
  let init context =
    let open World in
    let l = _gen_label context in
    spawn [l]
  in
  let updator context = 
    let open World in
    let new_label = _gen_label context in
    update_when Condition.(has_id Id.label_score) Updator.(replace_by new_label)
  in
  score, init, updator

let reset () = score := 0
