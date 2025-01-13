open Camel2d_renderable


let update_when condition f = List.map (fun entity ->
  if condition entity then f entity else entity
)
let remove_when condition = List.filter (fun entity -> not (condition entity))
let count_when condition entities = List.length @@ List.filter condition entities

let (&&&) c1 c2 entity = c1 entity && c2 entity
let (|||) c1 c2 entity = c1 entity || c2 entity

let has_id target_id (Renderable {id; _}) = target_id = id
let is_in mx my (Renderable {x; y; w; h; _}) =
  x <= mx && mx <= x + w && y <= my && my <= y + h

let x_is_smaller_than xx (Renderable {x; _}) = x < xx

let check_collision (Renderable e1) (Renderable e2) =
  let cx1, cy1 = e1.x + e1.w / 2, e1.y + e1.h / 2 in
  let cx2, cy2 = e2.x + e2.w / 2, e2.y + e2.h / 2 in
  let dx, dy = Int.abs (cx1 - cx2), Int.abs (cy1 - cy2) in
  let lw, lh = (e1.w + e2.w) / 2, (e1.h + e2.h) / 2 in
  dx <= lw && dy <= lh

let update_x new_x (Renderable r) = Renderable {r with x = new_x}
let update_y new_y (Renderable r) = Renderable {r with y = new_y}

let show (Renderable r) = Renderable {r with is_visible = true}
let hide (Renderable r) = Renderable {r with is_visible = false}
