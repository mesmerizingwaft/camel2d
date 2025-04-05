module TextStyle = Camel2d_text_style
module Color = Camel2d_color

include Camel2d_world

type renderer
type 'a t = ('a, renderer) _t

let put_alpha alpha =
  let* state = get in
  put {state with alpha}

let set_color color =
  let* {text_style; _} as state = get in
  let text_style = {text_style with color} in
  put {state with text_style}

let set_outline outline =
  let* {text_style; _} as state = get in
  let text_style = {text_style with outline} in
  put {state with text_style}

module Image = struct
  let render_xy ~x ~y label =
    let* {context; bucket; alpha; _} = get in
    let image = Camel2d_resource.fetch_image bucket label in
    Camel2d_resource.Image.render ~alpha ~x ~y ~context image;
    return ()

  let render_xywh ~x ~y ~w ~h label =
    let* {context; bucket; alpha; _} = get in
    let image = Camel2d_resource.fetch_image bucket label in
    Camel2d_resource.Image.render ~alpha ~x ~y ~w:(Some w) ~h:(Some h) ~context image;
    return ()
end

module Anime = struct
  let frame_to_sec_of_100 frame =
    Camel2d_global.spf *. (float_of_int frame) *. 100.
    |> int_of_float

  let render_xy ~x ~y label frame =
    let* {context; bucket; alpha; _} = get in
    let anime = Camel2d_resource.fetch_anime bucket label in
    let sec_of_100 = frame_to_sec_of_100 frame in
    Camel2d_resource.Anime.render ~alpha ~x ~y ~context anime sec_of_100;
    return ()

  let render_xywh ~x ~y ~w ~h label frame =
    let* {context; bucket; alpha; _} = get in
    let anime = Camel2d_resource.fetch_anime bucket label in
    let sec_of_100 = frame_to_sec_of_100 frame in
    Camel2d_resource.Anime.render ~alpha ~x ~y ~w:(Some w) ~h:(Some h) ~context anime sec_of_100;
    return ()
end

module Text = struct
  open Js_of_ocaml

  let draw_outline ctx outline text x y =
    match outline with
    | TextStyle.NoOutline -> ()
    | Edging color ->
      ctx##save;
      ctx##.lineWidth := 3.;
      ctx##.fillStyle := Color.js_str_of color;
      ctx##strokeText (Js.string text) (float_of_int x) (float_of_int y);
      ctx##restore

  let draw ~x ~y text =
    let* {context; alpha; text_style; _} = get in
    TextStyle.with_style text_style alpha context (fun ctx ->
      draw_outline ctx text_style.outline text x y;
      ctx##fillText (Js.string text) (float_of_int x) (float_of_int y);
    );
    return ()

  let draw_centerized ~x ~y text =
    let* {context; alpha; text_style; _} = get in
    TextStyle.with_style text_style alpha context (fun ctx ->
      let width = let text = Js.string text in
        (ctx##measureText text)##.width
      in
      let x = x - int_of_float (width /. 2.) in
      let y = y - int_of_float (float_of_int text_style.pt /. 2.) in
      draw_outline ctx text_style.outline text x y;
      ctx##fillText (Js.string text) (float_of_int x) (float_of_int y);
    );
    return ()
end

module Symbol = struct
  open Js_of_ocaml

  let draw_circle ~x ~y ~radius ~color =
    let* {context; alpha; _} = get in
    let ctx = Camel2d_context.get_context2d context in
    let js_color = Color.js_str_of color in
    ctx##save;
    ctx##.globalAlpha := alpha;
    ctx##.lineWidth := 5.;
    ctx##.strokeStyle := js_color;
    ctx##beginPath;
    let x = float_of_int x in
    let y = float_of_int y in
    let rx = radius in
    let ry = radius in
    let rotation = 0. in
    let start_angle = 0. in
    let end_angle = 2. *. Float.pi in
    let counter_clockwise = Js._true in
    ctx##ellipse x y rx ry rotation start_angle end_angle counter_clockwise;
    ctx##stroke;
    ctx##restore;
    return ()

  let rotate_point ~angle (x, y) =
    let x' = x *. cos angle -. y *. sin angle in
    let y' = x *. sin angle +. y *. cos angle in
    x', y'

  let draw_eq_triangle ~cx ~cy ~start_angle ~color ~radius =
    let* {context; alpha; _} = get in
    let ctx = Camel2d_context.get_context2d context in
    let js_color = Color.js_str_of color in
    let cx = float_of_int cx in
    let cy = float_of_int cy in
    let span = Float.pi *. 2. /. 3. in
    ctx##save;
    ctx##.globalAlpha := alpha;
    ctx##.lineWidth := 5.;
    ctx##.fillStyle := js_color;
    ctx##beginPath;
    let points = List.map (fun i ->
      let r = start_angle +. span *. float_of_int i in
      rotate_point ~angle:r (radius, 0.)
    ) [0; 1; 2] |> Array.of_list in
    let x, y = points.(0) in
    ctx##moveTo (cx +. x) (cy +. y);
    for i = 1 to 2 do
      let x, y = points.(i) in
      ctx##lineTo (cx +. x) (cy +. y);
    done;
    ctx##fill;
    ctx##restore;
    return ()

  let draw_play_button ~x ~y ~radius ~color =
    let radius = float_of_int radius in
    draw_circle ~x ~y ~radius ~color
    >> draw_eq_triangle ~cx:x ~cy:y ~start_angle:0. ~color ~radius:(radius *. 2. /. 3.)
end