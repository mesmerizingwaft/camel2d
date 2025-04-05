type context = Camel2d_context.t

module LImage : sig
  type t
  val render : context -> t -> unit
  val create : x:int -> y:int -> Camel2d_resource.label -> t
  val create_wh : x:int -> y:int -> w:int -> h:int -> Camel2d_resource.label -> t
end = struct
  type t
  let render _ _ = ()
  let create ~x:_ ~y:_ _label = failwith "dummy"
  let create_wh ~x:_ ~y:_ ~w:_ ~h:_ _label = failwith "dummy"
end

module LText : sig
  type t
  val render : context -> t -> unit
end = struct
  type t
  let render _ _ = ()
end

module LPopupText : sig
  type t
  val render : context -> t -> unit
end = struct
  type t
  let render _ _ = ()
end