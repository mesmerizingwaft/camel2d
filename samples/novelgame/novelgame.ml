open Camel2d

let novel_game = Game.create ()

module BrandLogo : Scene.T = struct
  let sw, sh = novel_game.width, novel_game.height
  let cnt = ref 0

  module ResourceLabels = struct
    open Resource
    let logo = gen_label ()
  end

  let load_resources =
    let open Resource in
    let open ResourceLabels in
    set_image_root "/samples/novelgame/static/imgs/"
    >> load_image logo "logo.png"

  module Id = struct
    let logo = "logo"
  end

  let logo =
    let open Entity in
    let w , h = 239, 104 in
    let pos = (sw - w)/ 2, (sh - h) / 2 in
    SingleImage.create Id.logo ResourceLabels.logo ~pos ~size:(w, h)

  module Logo = (val Assets.Fading.(
    let f = linear_fadeinout ~frame_in:30 ~frame_out:90 in
    create "logo" logo ~f_curve:f ~max_frame:(60 * 3)
  ))

  let initialize context =
    let open World in
    put_ref cnt 0
    >> Logo.initialize context

  let handle_event _context _ev = World.return ()

  let update context =
    let open World in
    Logo.update context
    >> ifm (let* cnt' = use_ref cnt in return (cnt' > 60 * 3))
      (start_scene "title")
      (let* cnt' = use_ref cnt in
       cnt := cnt' + 1;
       World.return ())
end

module GameTitle : Scene.T = struct
  let _sw, _sh = novel_game.width, novel_game.height
  module ResourceLabels = struct
    open Resource
    let bg = gen_label ()
    let font_bebas = gen_label ()
  end

  let load_resources =
    let open Resource in
    let open ResourceLabels in
    set_image_root "/samples/novelgame/static/imgs/"
    >> load_image bg "title.png"
    >> set_font_root "/samples/novelgame/static/fonts/"
    >> load_font font_bebas "BebasNeue-Regular.ttf"

  module Id = struct
    let bg = "bg"
  end

  let initialize _context =
    let sw, sh = novel_game.width, novel_game.height in
    let bg =
      let open Entity in
      let pos = 0, 0 in
      let size = sw, sh in
      SingleImage.create Id.bg ResourceLabels.bg ~pos ~size
    in
    let title_text =
      let open Entity in
      let style =
        let pt = 50 in
        let color = TextLabel.RGBA (255, 255, 255, 1.) in
        let outline = TextLabel.Edging (RGBA (0, 0, 0, 1.)) in
        let font_face = Some "font_family_0" in
        TextLabel.create_style ~color ~outline ~font_face pt
      in
      let pos = sw / 2, sh / 2 - 100 in
      TextLabel.create ~style ~pos ~base_horizontal:BHCenter "title_text" "TITLE (fontface Bebas)"
    in
    let title_text2 =
      let open Entity in
      let style =
        let pt = 50 in
        let color = TextLabel.RGBA (255, 255, 255, 1.) in
        let outline = TextLabel.Edging (RGBA (0, 0, 0, 1.)) in
        TextLabel.create_style ~color ~outline pt
      in
      let pos = sw / 2, sh / 2 - 50 in
      TextLabel.create ~style ~pos ~base_horizontal:BHCenter "title_text" "TITLE (fontface: None)"
    in
    let open World in
    spawn [bg; title_text; title_text2]

  let handle_event _context ev =
    let open World in
    match ev with
    | Event.MouseDown _ | Event.KeyDown {key_code = 32} ->
      start_scene "prologue"
    | _ -> return ()

  let update _context =
    let open World in
    return ()

end

module GameOver : Scene.T = struct
  let sw, sh = novel_game.width, novel_game.height

  let cnt = ref 0

  module ResourceLabels = struct
    open Resource
    let se_gameover = gen_label ()
  end

  let load_resources =
    let open Resource in
    let open ResourceLabels in
    set_audio_root "/samples/novelgame/static/audio/"
    >> set_audio_mode SE
    >> load_audio se_gameover "gameover.mp3"

  module Id = struct
    let gameover_text = "gameover_text"
  end

  let gameover_text =
    let open Entity in
    let style =
      let pt = 50 in
      let color = TextLabel.RGBA (255, 255, 255, 1.) in
      let outline = TextLabel.Edging (RGBA (0, 0, 0, 1.)) in
      TextLabel.create_style ~color ~outline pt
    in
    let pos = sw / 2, sh / 2 - 100 in
    TextLabel.create ~style ~pos ~base_horizontal:BHCenter Id.gameover_text "Game Over"
  
  module GameOverText = (val Assets.Fading.(
    let f = linear_fadein in
    create "gameover_text" gameover_text ~f_curve:f ~max_frame:(60)
  ))

  let initialize context =
    let open World in
    put_ref cnt 0
    >> GameOverText.initialize context
    >> play_audio ResourceLabels.se_gameover

  let handle_event _context _ev = World.return ()

  let update context =
    let open World in
    GameOverText.update context
    >> ifm (let* cnt' = use_ref cnt in return (cnt' > 60 * 3))
      (start_scene "title")
      (let* cnt' = use_ref cnt in
      put_ref cnt (cnt' + 1))
end

let _ =
  Game.add_scene novel_game "brand_logo" (module BrandLogo);
  Game.add_scene novel_game "title" (module GameTitle);
  Game.add_scene novel_game "prologue" (Prologue.make novel_game);
  Game.add_scene novel_game "gameover" (module GameOver);
  start novel_game "brand_logo"