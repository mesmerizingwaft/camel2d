
type entity = Camel2d_entity.t
type arbitrator = Camel2d_arbitrator.t
type event_handler = Camel2d_eventhandler.t
type resource_bucket = Camel2d_resource.bucket

type t = {
  entities: entity list;
  arbitrator: arbitrator;
  event_handler: event_handler;
  resource_bucket: resource_bucket;
}

let create () =
  let entities = [] in
  let arbitrator = Camel2d_arbitrator.init in
  let event_handler = Camel2d_eventhandler. init in
  let resource_bucket = Camel2d_resource.create_bucket () in
  {entities; arbitrator; event_handler; resource_bucket}

let render (context: Camel2d_context.t) t =
  List.iter (fun e -> Camel2d_entity.render e context t.resource_bucket) t.entities

let load_scene (module Scene: Camel2d_scene.T) =
  let open Promise in
  Scene.load_resources () >>= fun resource_bucket ->
    let entities, arbitrator, event_handler = Scene.start () in
    return { entities; arbitrator; event_handler; resource_bucket }

let handle_events scenes t =
  let rec inner entities =
    match Camel2d_event.take () with
      | None -> Promise.return { t with entities }
      | Some ev -> begin
        match t.event_handler ev entities with
          | Update entities -> inner entities
          | LoadScene scene_name ->
            Camel2d_event.clear ();
            let scene = Hashtbl.find scenes scene_name in
            load_scene scene
        end
  in
  inner t.entities

let arbitrate scenes t =
  let result =
    match t.arbitrator t.entities with
      | Update entities ->
        Promise.return { t with entities }
      | LoadScene scene_name ->
        let scene = Hashtbl.find scenes scene_name in
        load_scene scene
  in
  result

