open Js_of_ocaml

let play_button x y =
  let size = 200 in
  let render (t: Camel2d_entity_types.t) context _bucket =
    let ctx = Camel2d_context.get_context2d context in
    ctx##save;
    ctx##.lineWidth := 5.;
    ctx##.strokeStyle := Js.string "rgb(200, 200, 200)";
    ctx##beginPath;
    let x = float_of_int t.x +. float_of_int t.w /. 2. in
    let y = float_of_int t.y +. float_of_int t.h /. 2. in
    let rx = float_of_int (t.w / 2) in
    let ry = float_of_int (t.h / 2) in
    ctx##ellipse x y rx ry 0. 0. (2. *. Float.pi) Js._true;
    ctx##stroke;
    ctx##beginPath;
    for i = 0 to 3 do
      let l = float_of_int (t.w / 3) in
      let span = Float.pi *. 2. /. 3. in
      let r, r' = span *. float_of_int i, span *. float_of_int ((i + 1) mod 3) in
      let x, x' = x +. l *. cos r, x +. l *. cos r' in
      let y, y' = y +. l *. sin r, y +. l *. sin r' in
      ctx##moveTo x y;
      ctx##lineTo x' y';
    done;
    ctx##stroke;
    ctx##restore;
  in
  let x = x - size / 2 in
  let y = y - size / 2 in
  let w = size in
  let h = size in
  Camel2d_entity_types.{
    id = "play_button";
    render = render;
    is_visible = true;
    x; y; w; h;
    z_index=0
  }

module Make(Params: sig
  val game: Camel2d_game.t
  val next_scene: string
end) : Camel2d_scene.T = struct
  let load_resources =
    Camel2d_resource.return ()

  let initialize _context =
    let x = Params.game.width / 2 in
    let y = Params.game.height / 2 in
    let btn = play_button x y in
    Camel2d_world.spawn [btn]

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
