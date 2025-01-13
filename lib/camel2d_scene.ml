type 'a world = 'a Camel2d_world.t
type context = Camel2d_context.t
type bucket = Camel2d_resource.bucket
type entity = Camel2d_entity.t
type event = Camel2d_event.t
type label = Camel2d_resource.label

module type T = sig
  val images : (label * string) list
  val audios : (label * string) list
  val initialize : context -> entity list
  val update : context -> unit world
  val handle_event : context -> event -> unit world
end

let load_images bucket (module Scene: T) =
  let open Camel2d_resource in
  Scene.images
  |> List.map (fun (label, path) ->
    Promise.(Image.load path >>= load ~bucket ~label))
  |> Promise.join

let load_audios context bucket (module Scene: T) =
  let open Camel2d_resource in
  Scene.audios
  |> List.map (fun (label, path) ->
    Promise.(Audio.load ~context path >>= load ~bucket ~label))
  |> Promise.join

let load_resources context (module Scene: T) =
  let open Promise in
  let bucket = Camel2d_resource.create_bucket () in
  load_images bucket (module Scene) >>= fun _ ->
  load_audios context bucket (module Scene) >>= fun _ ->
  Camel2d_event.clear ();
  return bucket

(*
let start context (module Scene: T) =
  let open Promise in
  let bucket = Camel2d_resource.create_bucket () in
  load_images bucket (module Scene) >>= fun _ ->
  load_audios context bucket (module Scene) >>= fun _ ->
  Camel2d_event.clear ();
  return Camel2d_world.(
    put_bucket bucket
    >> put_entities (Scene.initialize context)
    >> put_event_handler Scene.handle_event
    >> put_updator Scene.update
  )*)