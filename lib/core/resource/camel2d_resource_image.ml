open Js_of_ocaml


type t = Dom_html.imageElement Js.t

let load src =
  let img = Dom_html.createImg Dom_html.document in
  let promise, resolver = Promise.make () in
  img##.src := Js.string src;
  img##.onload := Dom_html.handler (fun _ ->
    Promise.resolve resolver img;
    Js._false
  );
  promise

let render ?(alpha=1.0) ~context ~x ~y ?(w=None) ?(h=None) (img: t) =
  let ctx = Camel2d_context.get_context2d context in
  let x, y = float_of_int x, float_of_int y in
  let w = Option.(map (fun w -> float_of_int w) w |> value ~default:(float_of_int img##.width)) in
  let h = Option.(map (fun h -> float_of_int h) h |> value ~default:(float_of_int img##.height)) in
  ctx##save;
  ctx##.globalAlpha := alpha;
  ctx##drawImage_withSize img x y w h;
  ctx##restore

let width_of (img: t) = img##.width
let height_of (img: t) = img##.height