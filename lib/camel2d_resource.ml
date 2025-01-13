open Ext
open Js_of_ocaml

type context = Camel2d_context.t

type t =
  | Image of Dom_html.imageElement Js.t
  | Audio of Dom_html.audioElement Js.t

type label = string

let create_label name = name

type bucket = (label, t) Hashtbl.t

let create_bucket () : bucket = Hashtbl.create 10
let fetch (bucket: bucket) name = Hashtbl.find bucket name
let load ~bucket ~label t =
  let _ = Hashtbl.add bucket label t in
  Promise.return ()

module Image = struct
  let load src =
    let img = Dom_html.createImg Dom_html.document in
    let promise, resolver = Promise.make () in
    img##.src := Js.string src;
    img##.onload := Dom_html.handler (fun _ ->
      Promise.resolve resolver (Image img);
      Js._false
    );
    promise

  let render bucket label context ~x ~y ~w ~h =
    match fetch bucket label with
      | Audio _ ->
        failwith @@ Printf.sprintf "resource [%s] is not renderable" label
      | Image img ->
        let ctx = Camel2d_context.get_context2d context in
        let x, y, w, h = float_of_int x, float_of_int y, float_of_int w, float_of_int h in
        ctx##drawImage_withSize img x y w h
end

module Audio = struct
  let resume (context: Camel2d_context.t) =
    context.audio_context##resume

  let load ~(context:context) ?(is_loop=true) path =
    let audio_context = context.audio_context in
    let e = Dom_html.(createAudio document) in
    e##.src := Js.string path;
    e##.loop := Js.bool is_loop;
    let promise, resolver = Promise.make () in
    Dom_html.addEventListener e Dom_html.Event.loadstart (Dom_html.handler (fun _ ->
      let source = audio_context##createMediaElementSource e in
      source##connect (audio_context##.destination :> audioNode Js.t);
      Promise.resolve resolver (Audio e);
      Js._false
    )) Js._false |> ignore;
    promise

  let play bucket label =
    match fetch bucket label with
      | Image _ ->
        failwith @@ Printf.sprintf "resource [%s] is not playable" label
      | Audio audio ->
        ignore @@ audio##play

  let stop bucket label =
    match fetch bucket label with
    | Image _ ->
      failwith @@ Printf.sprintf "resource [%s] is not playable" label
    | Audio audio ->
      ignore @@ audio##pause;
      audio##.currentTime := 0.
end
