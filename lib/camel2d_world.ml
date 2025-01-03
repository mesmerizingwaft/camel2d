
type entity = Camel2d_entity.t
type arbitrator = Camel2d_arbitrator.t
type resource_bucket = Camel2d_resource.bucket

type t = {
  entities: entity list;
  arbitrator: arbitrator;
  resource_bucket: resource_bucket;
}

let create () =
  let entities = [] in
  let arbitrator = Camel2d_arbitrator.do_nothing in
  let resource_bucket = Camel2d_resource.create_bucket () in
  {entities; arbitrator; resource_bucket}

let render (context: Camel2d_context.t) t =
  List.iter (fun e -> Camel2d_entity.render e context t.resource_bucket) t.entities

let load_scene (module Scene: Camel2d_scene.T) =
  let open Promise in
  Scene.load_resources () >>= fun resource_bucket ->
    Camel2d_event.clear ();
    let entities, arbitrator = Scene.start () in
    return { entities; arbitrator; resource_bucket }
  
let arbitrate scenes t =
  let result =
    match t.arbitrator t.entities with
      | Camel2d_arbitrator.Discharge (entities, arbitrator, resource_bucket) ->
        Promise.return { entities; arbitrator; resource_bucket }
      | Update entities ->
        Promise.return { t with entities }
      | LoadScene scene_name ->
        let scene = Hashtbl.find scenes scene_name in
        load_scene scene
  in
  Camel2d_event.clear ();
  result

