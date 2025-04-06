open Js_of_ocaml
open Js

class type fontFaceElement = object
    method family : js_string t readonly_prop
    method style : js_string t readonly_prop
    method weight : js_string t readonly_prop
    method stretch : js_string t readonly_prop
    method unicodeRange : js_string t readonly_prop
    method variant : js_string t readonly_prop
    method featureSettings : js_string t readonly_prop
    method display : js_string t readonly_prop
    method src : js_string t readonly_prop
    method status : js_string t readonly_prop
    method load : unit -> 'a meth
end

class type fontFaceSet = object
  method add : fontFaceElement Js.t -> unit meth
  method check : js_string t -> js_string t -> bool meth
  method delete : fontFaceElement Js.t -> unit meth
  method load : unit -> 'a meth
  method ready : bool readonly_prop
  method status : js_string t readonly_prop
end

let create_font_face (family: js_string Js.t) (source: js_string Js.t) =
  let family = Unsafe.coerce family in
  let source = Unsafe.coerce source in
  Unsafe.coerce @@ Unsafe.new_obj (Unsafe.pure_js_expr "FontFace") [|family; source|]

let load_and_then (font_face: fontFaceElement Js.t) ?(f_err=fun () -> ()) (f: unit -> unit)  =
  let f = Unsafe.inject @@ Js.wrap_meth_callback f in
  let f_err = Unsafe.inject @@ Js.wrap_meth_callback f_err in
  let js_promise = font_face##load () in
  ignore @@ Unsafe.meth_call js_promise "then" [|Unsafe.coerce f|];
  Unsafe.meth_call js_promise "catch" [|Unsafe.coerce f_err|]