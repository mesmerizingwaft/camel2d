open Ext
open Js_of_ocaml

type t = string

let gen_name =
  (* TODO : replace_by_UUID *)
  let cnt = ref 0 in
  fun () ->
    let name =
      "font_family_" ^ (string_of_int !cnt)
    in
    cnt := !cnt + 1;
    name


let load src =
  let name = gen_name () in
  let ff = Css_font.create_font_face
    (Js.string name)
    (Js.string @@ Printf.sprintf "url(%s)" src)
  in
  let promise, resolver = Promise.make () in
  ignore @@ Css_font.load_and_then ff ~f_err:(fun () ->
    let open Camel2d_error_types in
    Promise.reject resolver ((ResourceNotAvailable ("fonts", src)))
  ) (fun _ ->
    let document_fonts = Js.Unsafe.get Dom_html.document "fonts" in
    ignore @@ Js.Unsafe.meth_call document_fonts "add" [|ff|];
    Promise.resolve resolver name;
    ()
  );
  promise

let font_family_of t = t
