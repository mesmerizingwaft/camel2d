open Js_of_ocaml

module MouseButtonType = struct
  type t =
    | Left
    | Right
    | Middle
end

module MouseUp = struct
  type t = {
    button_type: MouseButtonType.t;
    x: int;
    y: int;
  }
end

module MouseDown = struct
  type t = {
    button_type: MouseButtonType.t;
    x: int;
    y: int;
  }
end

module MouseClick = struct
  type t = {
    button_type: MouseButtonType.t;
    x: int;
    y: int;
  }
end

module MouseMove = struct
  type t = {
    x: int;
    y: int;
  }
end

type t =
  | MouseUp of MouseUp.t
  | MouseDown of MouseDown.t
  | MouseClick of MouseClick.t
  | MouseMove of MouseMove.t

let _stack : t Queue.t = Queue.create ()

let _push ev =
  if Queue.length _stack >= 50 then Queue.clear _stack;
  match ev with
    | MouseUp t -> begin
      match Queue.take_opt _stack with
        | None -> Queue.push ev _stack
        | Some (MouseDown prev) when
          prev.button_type = t.button_type
          && prev.x = t.x
          && prev.y = t.y ->
            let button_type = t.button_type in
            let x, y = t.x, t.y in
            Queue.push (MouseClick {button_type; x; y}) _stack
        | Some prev -> Queue.push prev _stack; Queue.push (MouseUp t) _stack
      end
    | ev -> Queue.push ev _stack

let init (context: Camel2d_context.t) () =
  let canvas = context.html_canvas in
  let handler = Dom_html.handler (fun e ->
    let x, y = Ext.scale_mouse_pointer canvas e##.clientX e##.clientY in
    let button_type = match e##.button with
      | 0 -> MouseButtonType.Left
      | 1 -> Middle
      | 2 -> Right
      | _ -> failwith "unimplemented" in
    let ev = MouseUp {button_type; x; y} in
    _push ev;
    Js._false
  ) in
  Dom_html.addEventListener canvas Dom_html.Event.mouseup handler Js._false
  |> ignore;
  let handler = Dom_html.handler (fun e ->
    let x, y = e##.offsetX, e##.offsetY in
    let button_type = match e##.button with
      | 0 -> MouseButtonType.Left
      | 1 -> Middle
      | 2 -> Right
      | _ -> failwith "unimplemented" in
    let ev = MouseDown {button_type; x; y} in
    _push ev;
    Js._false
  ) in
  Dom_html.addEventListener canvas Dom_html.Event.mousedown handler Js._false
  |> ignore;
  let handler = Dom_html.handler (fun e ->
    let x, y = Ext.scale_mouse_pointer canvas e##.clientX e##.clientY in
    let ev = MouseMove {x; y} in
    _push ev;
    Js._false
  ) in
  Dom_html.addEventListener canvas Dom_html.Event.mousemove handler Js._false
  |> ignore

let take () = Queue.take_opt _stack
let clear () = Queue.clear _stack