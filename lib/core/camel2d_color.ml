
type t =
  | RGB of int * int * int
  | RGBA of int * int * int * float

let js_str_of color =
  let open Js_of_ocaml in
  match color with
    | RGB (r, g, b) -> Js.string @@ Printf.sprintf "rgb(%d,%d,%d)" r g b
    | RGBA (r, g, b, a) -> Js.string @@ Printf.sprintf "rgba(%d,%d,%d,%f)" r g b a
