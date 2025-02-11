
module SimpleAgreement4BGM = struct
  module Make(Params: sig
    val game: Camel2d_game.t
    val message: string
    val pt: int
    val next_scene: string
  end) : Camel2d_scene.T = struct
    let load_resources = Camel2d_resource.return ()
    
    let initialize context =
      let message =
        let open Camel2d_entity.Renderable in
        let open TextLabel in
        let style =
          let outline = Edging (RGBA (0, 0, 0, 1.)) in
          create_style ~outline Params.pt
        in
        let pos = (Params.game.width / 2, (Params.game.height - Params.pt) / 2) in
        create ~context ~style ~pos ~base_horizontal:BHCenter "message" Params.message
      in
      let open Camel2d_world in
      spawn_r [message]
  
    let handle_event context ev =
      let open Camel2d_world in
      match ev with
        | Camel2d_event.MouseUp _ ->
          Camel2d_resource.Audio.resume context;
          start_scene Params.next_scene
        | _ ->
          return ()
  
    let update _ = Camel2d_world.return ()
  end
end
