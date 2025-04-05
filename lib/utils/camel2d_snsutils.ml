open Js_of_ocaml

let tweet ?(url=None) ?(hashtags=[]) text =
  let a = Dom_html.createA Dom_html.document in
  let hashtags = List.map (fun t -> Js.to_string @@ Js.encodeURIComponent @@ Js.string t) hashtags in
  let hashtags = String.concat "," hashtags in
  let hashtags = if String.length hashtags > 0 then "&hashtags=" ^ hashtags else hashtags in
  let url = Option.map (fun url -> Js.to_string @@ Js.encodeURIComponent @@ Js.string url) url in
  let url = match url with None -> "" | Some url -> "&url=" ^ url in
  let query = Printf.sprintf
    "https://twitter.com/intent/tweet?text=%s%s%s"
    (Js.to_string (Js.encodeURIComponent (Js.string text)))
    url
    hashtags
  in
  a##.href := Js.string query;
  a##.target := Js.string "_blank";
  a##click