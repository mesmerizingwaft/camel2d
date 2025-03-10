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

let render img context ~x ~y ~w ~h =
  let ctx = Camel2d_context.get_context2d context in
  let x, y, w, h = float_of_int x, float_of_int y, float_of_int w, float_of_int h in
  ctx##drawImage_withSize img x y w h

