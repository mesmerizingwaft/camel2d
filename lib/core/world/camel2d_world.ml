module Context = Camel2d_context
module Resource = Camel2d_resource
module TextStyle = Camel2d_text_style

type state = {
  context: Context.t;
  bucket: Resource.bucket;
  alpha: float;
  text_style: TextStyle.t;
}
type 'a next_scene =
  | NewScene of string
  | Continue of 'a * state
type ('a, 'mode) _t = state -> 'a next_scene

let new_state context bucket = {
    context;
    bucket;
    alpha=1.0;
    text_style = TextStyle.init
  }
let return a = fun state -> Continue (a, state)
let run ~state t = t state
let bind t f = fun state ->
  match t state with
    | NewScene name -> NewScene name
    | Continue (a, state) -> f a state
let map t f = bind t (fun a -> return (f a))

let (>>=) t f = bind t f
let (let*) t f = bind t f
let (let+) t f = map t f
let (>>) t1 t2 = let* _ = t1 in t2

let put state = fun _ -> Continue ((), state)
let get = fun state -> Continue (state, state)

let set_font_size pt =
  let* {text_style; _} as state = get in
  let text_style = {text_style with pt} in
  put {state with text_style}

let put_font_face font_face =
  let* {text_style; _} as state = get in
  let text_style = {text_style with font_face = Some font_face} in
  put {state with text_style}

let set_letter_spacing letter_spacing =
  let* {text_style; _} as state = get in
  let text_style = {text_style with letter_spacing} in
  put {state with text_style}

let set_font_face label =
  let* {bucket; _} = get in
  let font = Camel2d_resource.fetch_font bucket label in
  let font = Camel2d_resource.Font.font_family_of font in
  put_font_face font

let clear_text_style =
  let* state = get in
  let text_style = TextStyle.init in
  put {state with text_style}

let get_canvas_size = let+ {context; _} = get in
  Camel2d_context.get_canvas_size context

let get_text_size text =
  let* {context; text_style; alpha; _} = get in
  TextStyle.with_style text_style alpha context (fun ctx ->
    let open Js_of_ocaml in
    let width = let text = Js.string text in
      (ctx##measureText text)##.width
    in (int_of_float width, text_style.pt)
  ) |> return

let get_image_size label =
  let* {bucket; _} = get in
  let img = Camel2d_resource.fetch_image bucket label in
  let w = Camel2d_resource.Image.width_of img in
  let h = Camel2d_resource.Image.height_of img in
  return (w, h)

let get_anime_size label msec =
  let* {bucket; _} = get in
  let anime = Camel2d_resource.fetch_anime bucket label in
  let w = Camel2d_resource.Anime.width_of anime msec in
  let h = Camel2d_resource.Anime.height_of anime msec in
  return (w, h)

let print_endline msg =
  let+ _ = return () in
  print_endline msg