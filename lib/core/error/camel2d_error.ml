open Js_of_ocaml
open Camel2d_error_types

let notify_critical_error msg =
  let bg_critical_error = Dom_html.createDiv Dom_html.document in
  bg_critical_error##.style##.top := Js.string "0px";
  bg_critical_error##.style##.left := Js.string "0px";
  bg_critical_error##.style##.width := Js.string "100vw";
  bg_critical_error##.style##.height := Js.string "120vh";
  bg_critical_error##.style##.backgroundColor := Js.string "rgba(20,20,20,0.5)";
  bg_critical_error##.style##.position := Js.string "absolute";
  bg_critical_error##.style##.display := Js.string "block";
  let critical_error = Dom_html.createDiv Dom_html.document in
  critical_error##.style##.width := Js.string "500px";
  critical_error##.style##.height := Js.string "200px";
  critical_error##.style##.backgroundColor := Js.string "white";
  critical_error##.style##.position := Js.string "relative";
  critical_error##.style##.top := Js.string "30vh";
  critical_error##.style##.marginLeft := Js.string "auto";
  critical_error##.style##.marginRight := Js.string "auto";
  critical_error##.style##.paddingLeft := Js.string "10px";
  critical_error##.style##.paddingTop := Js.string "10px";
  critical_error##.style##.borderRadius := Js.string "10px";
  let header = Dom_html.createDiv Dom_html.document in
  header##.style##.fontSize := Js.string "20px";
  header##.style##.fontWeight := Js.string "bold";
  header##.style##.color := Js.string "red";
  header##.textContent := Js.some @@ Js.string "\u{26a0} Critical error";
  let message_body = Dom_html.createDiv Dom_html.document in
  message_body##.style##.fontSize := Js.string "16px";
  message_body##.style##.color := Js.string "black";
  message_body##.style##.paddingLeft := Js.string "2px";
  message_body##.style##.paddingTop := Js.string "2px";
  message_body##.style##.paddingBottom := Js.string "2px";
  message_body##.style##.paddingRight := Js.string "2px";
  message_body##.textContent := Js.some @@ Js.string msg;
  Dom.appendChild critical_error header;
  Dom.appendChild critical_error message_body;
  Dom.appendChild bg_critical_error critical_error;
  Dom.appendChild Dom_html.document##.body bg_critical_error

let error_presenter f =
  try
    f ()
  with
    | ResourceNotLoaded resource_type ->
      let msg = Printf.sprintf "Some resource label was refered before it was loaded. Please check if all the resource labels are loaded before the scene loaded. (resource_type: %s)" resource_type in
      notify_critical_error msg;
      failwith msg
    | ResourceNotAvailable (resource_type, resource_path) ->
      let msg = Printf.sprintf "Failed to load the resource (%s) (resource_type: %s). Please, make sure the path is correct and the server host the resource" resource_path resource_type in
      notify_critical_error msg;
      failwith msg