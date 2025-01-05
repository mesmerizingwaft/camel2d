open Js_of_ocaml

type context = Camel2d_context.t
type resource_bucket = Camel2d_resource.bucket

type t = E : {
  id: string;
  render: context -> resource_bucket -> unit;
  x: int ref;
  y: int ref;
  w: int ref;
  h: int ref;
} -> t

let render (E {render; _}) = render

let update_when condition f = List.map (fun entity ->
  if condition entity then f entity else entity
)

let (&&&) c1 c2 entity = c1 entity && c2 entity

let has_id target_id (E {id; _}) = target_id = id
let is_in mx my (E {x; y; w; h; _}) =
  !x <= mx && mx <= !x + !w && !y <= my && my <= !y + !h

let check_collision (E e1) (E e2) =
  let cx1, cy1 = !(e1.x) + !(e1.w) / 2, !(e1.y) + !(e1.h) / 2 in
  let cx2, cy2 = !(e2.x) + !(e2.w) / 2, !(e2.y) + !(e2.h) / 2 in
  let dx, dy = Int.abs (cx1 - cx2), Int.abs (cy1 - cy2) in
  let lw, lh = (!(e1.w) + !(e2.w)) / 2, (!(e1.h) + !(e2.h)) / 2 in
  dx <= lw && dy <= lh

let create_from_image id resource_name ~pos ~size =
  let (x_, y_), (w_, h_) = pos, size in
  let x, y, w, h = ref x_, ref y_, ref w_, ref h_ in
  let render context resource_bucket =
    let img = Camel2d_resource.fetch_img resource_bucket resource_name in
    let ctx = Camel2d_context.get_context2d context in
    let x, y = let xi, yi = !x, !y in float_of_int xi, float_of_int yi in
    let w, h = let wi, hi = !w, !h in float_of_int wi, float_of_int hi in
    let src_x, src_y, src_w, src_h = 0., 0., float_of_int img##.width, float_of_int img##.height in
    ctx##drawImage_full img src_x src_y src_w src_h x y w h
  in
  E { id; render; x; y; w; h }

let create_from_text
  ?(color=`RGBA(255, 255, 255, 1.))
  ?(outline=`NoOutline)
  ?(font_face=None)
  ?(letter_spacing="normal")
  ?(base_horizontal=`Left)
  ~pt ~pos ~size id text =
  let (x_, y_), (w_, h_) = pos, size in
  let x, y, w, h = ref x_, ref y_, ref w_, ref h_ in
  let render context _ =
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
      then !x - (int_of_float ((ctx##measureText (Js.string text))##.width /. 2.))
      else !x
    in
    ctx##.textBaseline := Js.string "top";
    Ext.set_letter_spacing_of ctx letter_spacing;
    (match outline with
    | `NoOutline -> ()
    | `Edging (r,g,b) ->
      ctx##save;
      ctx##.lineWidth := 3.;
      ctx##.fillStyle := Js.string @@ Printf.sprintf "rgb(%d,%d,%d)" r g b;
      ctx##strokeText (Js.string text) (float_of_int x) (float_of_int !y);
      ctx##restore;
    );
    ctx##fillText (Js.string text) (float_of_int x) (float_of_int !y);
    ctx##restore;
  in
  E { id; render; x; y; w; h }