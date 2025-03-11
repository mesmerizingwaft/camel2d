open Js_of_ocaml
open Camel2d_entity_types

type t = Camel2d_entity_types.t
module SingleImage = Camel2d_entity_single_image
module TextLabel = Camel2d_entity_text_label

let id_of e = e.id
let is_visible e = e.is_visible
let has_id target_id {id; _} = target_id = id
let render context resource_bucket ({render; _} as t) =
  render t context resource_bucket
let show r = {r with is_visible = true}
let hide r = {r with is_visible = false}
let update_x new_x r = {r with x = new_x}
let update_y new_y r = {r with y = new_y}
let update_x_by_diff diff r = {r with x = r.x + diff}
let update_y_by_diff diff r = {r with y = r.y + diff}
let x_is_smaller_than xx {x; _} = x < xx
let is_in mx my {x; y; w; h; _} =
  x <= mx && mx <= x + w && y <= my && my <= y + h
let check_collision e1 e2 =
  let cx1, cy1 = e1.x + e1.w / 2, e1.y + e1.h / 2 in
  let cx2, cy2 = e2.x + e2.w / 2, e2.y + e2.h / 2 in
  let dx, dy = Int.abs (cx1 - cx2), Int.abs (cy1 - cy2) in
  let lw, lh = (e1.w + e2.w) / 2, (e1.h + e2.h) / 2 in
  dx <= lw && dy <= lh
let compare_z a b = a.z_index - b.z_index

let create_from_text
  ?(is_visible=true)
  ?(color=`RGBA(255, 255, 255, 1.))
  ?(outline=`NoOutline)
  ?(font_face=None)
  ?(letter_spacing="normal")
  ?(base_horizontal=`Left)
  ?(z_index=0)
  ~pt ~pos ~size id text =
  let (x, y), (w, h) = pos, size in
  let render {x; y; is_visible; _} context _ =
    if is_visible then begin
      let ctx = Camel2d_context.get_context2d context in
      ctx##save;
      ctx##.fillStyle := begin
        match color with
          | `RGBA (r,g,b,a) ->
            Js.string @@ Printf.sprintf "rgba(%d,%d,%d,%f)" r g b a
          | _ -> failwith "invalid color"
      end;
      let pt = Printf.sprintf "%dpx" pt in
      let font_face = Option.value font_face ~default:"caption" in
      ctx##.font := Js.string @@ Printf.sprintf "%s %s" pt font_face;
      let x = if base_horizontal = `Center
        then x - (int_of_float ((ctx##measureText (Js.string text))##.width /. 2.))
        else x
      in
      ctx##.textBaseline := Js.string "top";
      Ext.set_letter_spacing_of ctx letter_spacing;
      (match outline with
      | `NoOutline -> ()
      | `Edging (r,g,b) ->
        ctx##save;
        ctx##.lineWidth := 3.;
        ctx##.fillStyle := Js.string @@ Printf.sprintf "rgb(%d,%d,%d)" r g b;
        ctx##strokeText (Js.string text) (float_of_int x) (float_of_int y);
        ctx##restore;
      );
      ctx##fillText (Js.string text) (float_of_int x) (float_of_int y);
      ctx##restore
    end
  in
  { id; render; is_visible; x; y; w; h; z_index }
