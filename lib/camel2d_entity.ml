
type t =
  | R of Camel2d_renderable.t
  | P of Camel2d_playable.t

let render context bucket = function
  | P _ -> ()
  | R r -> Camel2d_renderable.render context bucket r

let renderables_of t = List.filter_map (function R r -> Some r | _ -> None) t
let playables_of t = List.filter_map (function P p -> Some p | _ -> None) t

module Renderable = Camel2d_renderable
module Playable = Camel2d_playable