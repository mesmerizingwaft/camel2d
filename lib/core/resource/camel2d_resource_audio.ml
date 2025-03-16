open Ext
open Js_of_ocaml

type context = Camel2d_context.t

type t = {
  audio : Dom_html.audioElement Js.t;
  is_ended : bool ref
}

let _buff = ref []

let resume (context: Camel2d_context.t) =
  (Camel2d_context.get_audio_context context)##resume

let load ~(context:context) ?(is_loop=true) path =
  let audio_context = Camel2d_context.get_audio_context context in
  let audio = Dom_html.(createAudio document) in
  _buff := audio::!_buff;
  audio##.src := Js.string path;
  audio##.loop := Js.bool is_loop;
  let promise, resolver = Promise.make () in
  Dom_html.addEventListener audio Dom_html.Event.loadstart (Dom_html.handler (fun _ ->
    let source = audio_context##createMediaElementSource audio in
    source##connect (audio_context##.destination :> audioNode Js.t);
    Promise.resolve resolver {audio; is_ended=ref false};
    Js._false
  )) Js._false |> ignore;
  promise

let play {audio; is_ended} =
  let handler = Dom_html.handler (fun _ ->
    print_endline (Printf.sprintf "se ended (%s)" (Js.to_string audio##.src));
    is_ended := true;
    audio##.currentTime := 0.;
    Js._false
  ) in
  ignore @@ Dom_html.addEventListener audio Dom_html.Event.ended handler Js._false;
  ignore @@ audio##play

let stop {audio; _} =
  ignore @@ audio##pause;
  audio##.currentTime := 0.

let stop_all () =
  let stop node =
    let node = Js.Unsafe.coerce node in
    ignore @@ node##pause;
    node##.currentTime := 0.
  in
  List.iter stop !_buff;
  _buff := []

let is_ended {is_ended; _} = !is_ended
