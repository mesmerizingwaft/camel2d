open Ext
open Js_of_ocaml


type t = {
  html_canvas: Dom_html.canvasElement Js.t;
  audio_context: audioContext Js.t option
}

let create () =
  let html_canvas =
    try
      Option.get @@
        Dom_html.getElementById_coerce "canvas-main" Dom_html.CoerceTo.canvas
    with Invalid_argument _ ->
      failwith @@ Printf.sprintf "Fatal error: canvas[id='canvas-main'] did not exist"
  in
  let audio_context = Some new%js audioContext in
  {html_canvas; audio_context}

let get_context2d t = (t.html_canvas)##getContext Dom_html._2d_ 
let cleanup_canvas (t: t) =
  let context = get_context2d t in
  let w, h = context##.canvas##.width, context##.canvas##.height in
  let w, h = float_of_int w, float_of_int h in
  context##clearRect 0. 0. w h;
  context##.fillStyle := Js.string "rgb(0, 0, 0)";
  context##fillRect 0. 0. w h;
  ()

let get_audio_context t = Option.get t.audio_context

let dummy_context () =
  let canvas = Dom_html.createCanvas Dom_html.document in
  {html_canvas=canvas; audio_context=None}