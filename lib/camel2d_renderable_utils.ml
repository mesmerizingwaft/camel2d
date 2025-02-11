open Camel2d_entity.Renderable


let update_when condition f = List.map (fun entity ->
  if condition entity then f entity else entity
)
let remove_when condition = List.filter (fun entity -> not (condition entity))
let count_when condition entities = List.length @@ List.filter condition entities

let (&&&) c1 c2 entity = c1 entity && c2 entity
let (|||) c1 c2 entity = c1 entity || c2 entity

let has_id target_id {id; _} = target_id = id
let is_in mx my {x; y; w; h; _} =
  x <= mx && mx <= x + w && y <= my && my <= y + h

let x_is_smaller_than xx {x; _} = x < xx

let check_collision e1 e2 =
  let cx1, cy1 = e1.x + e1.w / 2, e1.y + e1.h / 2 in
  let cx2, cy2 = e2.x + e2.w / 2, e2.y + e2.h / 2 in
  let dx, dy = Int.abs (cx1 - cx2), Int.abs (cy1 - cy2) in
  let lw, lh = (e1.w + e2.w) / 2, (e1.h + e2.h) / 2 in
  dx <= lw && dy <= lh

let update_x new_x r = {r with x = new_x}
let update_y new_y r = {r with y = new_y}
let update_x_by_diff diff r = {r with x = r.x + diff}
let update_y_by_diff diff r = {r with y = r.y + diff}

let show r = {r with is_visible = true}
let hide r = {r with is_visible = false}
