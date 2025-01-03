module type Scene = Camel2d_scene.T

let no_loading_screen scene = scene

(*
let gen_loading_screen resources entities =
  let module Loading : Scene = struct
    
  end
  in (module Loading: Scene)


  let wait_loading (module NextScene: T) =
    let module SceneLoading: T = struct
      let _next_resources = ref None
  
      let load_resources () =
        return @@ Camel2d_resource.create_bucket ()
      let load_entities () = []
      let arbitrator _ entities =
        match !_next_resources with
          | Some next_resources when is_resolved next_resources ->
            Camel2d_arbitrator.Discharge (run n)
  
  
      let next_entities = ref []
      let load_entities () =
        next_entities := NextScene.load_entities ();
        []
      let arbitrator _ entities =
        match !next_entities with
          | Some next_entities when is_resolved next_entities ->
            Camel2d_arbitrator.Discharge (run next_entities, NextScene.arbitrator)
          | _ ->
            Camel2d_arbitrator.Update entities
    end in
    (module SceneLoading: T)*)