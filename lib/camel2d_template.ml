
module SimpleAgreement4BGM = struct
  module Make(Params: sig
    val game: Camel2d_game.t
    val message: string
    val pt: int
  end) : Camel2d_scene.T = struct
    let audios = []
    let images = []
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
      Camel2d_entity.[R message]
  
    let handle_event context ev =
      let open Camel2d_world in
      match ev with
        | Camel2d_event.MouseUp _ ->
          Camel2d_resource.Audio.resume context;
          start_scene "main"
        | _ ->
          return ()
  
    let update _ = Camel2d_world.return ()
  end
end
