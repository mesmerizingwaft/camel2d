
type 'a state =
  | Pending
  | Resolved of 'a
  | Rejected of exn

type 'a handler = 'a state -> unit

type 'a promise = {
  mutable state: 'a state;
  mutable handlers: 'a handler list
}
type 'a resolver = 'a promise

let enqueue
  (handler: 'a state -> unit)
  (promise: 'a promise): unit =
  promise.handlers <- handler :: promise.handlers

let make () =
  let p = {state = Pending; handlers = []} in
  p, p

let return a = {state = Resolved a; handlers = []}

let state p = p.state

let write_once p s =
  if p.state = Pending then p.state <- s
  else invalid_arg "cannot write twice"

let resolve_or_reject (r: 'a resolver) (st: 'a state) =
  assert (st <> Pending);
  let handlers = r.handlers in
  r.handlers <- [];
  write_once r st;
  List.iter (fun f -> f st) handlers

let resolve r x = resolve_or_reject r (Resolved x)

let reject r x = resolve_or_reject r (Rejected x)

let run p = match p.state with
  | Resolved a -> a
  | Rejected exn -> raise exn
  | Pending -> invalid_arg "Unwrapping a pending promise"

let handler resolver : 'a handler = function
  | Pending -> failwith "handler RI violated."
  | Resolved a -> resolve resolver a
  | Rejected exn -> reject resolver exn

let handler_of_callback callback resolver: 'a handler = function
  | Pending -> failwith "handler RI violated"
  | Rejected exn -> reject resolver exn
  | Resolved a ->
    let promise = callback a in
    match promise.state with
      | Resolved a -> resolve resolver a
      | Rejected exn -> reject resolver exn
      | Pending ->
        enqueue (handler resolver) promise 

let (>>=) (m: 'a promise) (f: 'a -> 'b promise): 'b promise =
  match m.state with
    | Resolved a -> f a
    | Rejected exn -> {state=(Rejected exn); handlers=[]}
    | Pending ->
      let output_promise, output_resolver = make () in
      enqueue (handler_of_callback f output_resolver) m;
      output_promise

let (let*) t f = t >>= f

let rec join (ps: unit promise list): unit promise =
  match ps with
    | [] -> return ()
    | p::ps -> p >>= (fun () -> join ps)

let _ignore = ignore

let ignore (p: 'a promise): unit promise = p >>= (fun _ -> return ())

let is_resolved p = match state p with
  | Resolved _ -> true
  | _ -> false

let str_of_state = function
  | Pending -> "Pending"
  | Resolved _ -> "Resolved"
  | Rejected _ -> "Rejected"

let continue_after_resolved p k =
  let open Js_of_ocaml in
  let rec inner p =
    if is_resolved @@ ignore p then k (run p)
    else
      let inner = Js.wrap_callback (fun () -> inner p) in
      _ignore @@ Dom_html.window##setTimeout inner 15.
  in inner p

module List = struct
  include List
  let rec sequence: 'a promise list -> 'a list promise = function
    | [] -> return []
    | p::ps ->
      p >>= fun a ->
      sequence ps >>= fun aa ->
      return (a::aa)
end
