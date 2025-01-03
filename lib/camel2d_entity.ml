open Js_of_ocaml

type context = Camel2d_context.t
type resource_bucket = Camel2d_resource.bucket

type t = E : {
  id: string;
  render: context -> resource_bucket -> unit;
  pos: int * int;
  size: int * int;
} -> t

let render (E {render; _}) = render

let is_in (E {pos; size; _}) mx my =
  let (x, y), (w, h) = pos, size in
  x <= mx && mx <= x + w && y <= my && my <= y + h

let create_from_image id resource_name ~pos ~size =
  let render context resource_bucket =
    let img = Camel2d_resource.fetch_img resource_bucket resource_name in
    let ctx = Camel2d_context.get_context2d context in
    let x, y = let xi, yi = pos in float_of_int xi, float_of_int yi in
    let w, h = let wi, hi = size in float_of_int wi, float_of_int hi in
    let src_x, src_y, src_w, src_h = 0., 0., float_of_int img##.width, float_of_int img##.height in
    ctx##drawImage_full img src_x src_y src_w src_h x y w h
  in
  E {
    id;
    render;
    pos;
    size
  }

let create_from_text
  ?(color=`RGBA(255, 255, 255, 1.))
  ?(outline=`NoOutline)
  ?(font_face=None)
  ?(letter_spacing="normal")
  ~pt ~pos ~size id text =
  let render context _ =
    let ctx = Camel2d_context.get_context2d context in
    let x, y = pos in
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
    ctx##restore;
  in
  E {
    id;
    render;
    pos;
    size
  }