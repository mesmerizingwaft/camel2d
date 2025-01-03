open Js_of_ocaml

type t =
  | Image of Dom_html.imageElement Js.t

type bucket = (string, t) Hashtbl.t

let create_bucket () : bucket = Hashtbl.create 10

let load_img src =
  let img = Dom_html.createImg Dom_html.document in
  let promise, resolver = Promise.make () in
  img##.src := Js.string src;
  img##.onload := Dom_html.handler (fun _ ->
    Promise.resolve resolver (Image img);
    Js._false
  );
  promise

let fetch (bucket: bucket) name =
  Hashtbl.find bucket name

let fetch_img bucket name =
  match fetch bucket name with
    | Image img -> img
(*    | _ -> failwith "Invalid resource" *)
