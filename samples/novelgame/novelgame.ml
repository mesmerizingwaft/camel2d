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
  let sw, sh = novel_game.width, novel_game.height
  module ResourceLabels = struct
    open Resource
    let bg = gen_label ()
    let bg_message_box = gen_label ()
  end

  let load_resources =
    let open Resource in
    let open ResourceLabels in
    set_image_root "/samples/novelgame/static/imgs/"
    >> load_image bg "bg_title.png"
    >> load_image bg_message_box "hakkou2.png"

  let bg_message_box = 
    let open Entity in
    let w, h = sw - 20, 200 in
    let pos = 10, sh - 210 in
    SingleImage.create "bg_message_box" ResourceLabels.bg_message_box ~pos ~size:(w, h)

  let {
    Assets.MessageWindow.put_text;
    clear_text;
    m
  } = Assets.MessageWindow.(
    let style = Entity.TextLabel.(create_style ~color:(RGBA (255, 255, 255, 1.)) 40) in
    create bg_message_box ~style ~padding_l:30 ~padding_t:30 ~padding_r:30 ~padding_b:30
  )

  module MessageWindow = (val m)

  module Id = struct
    let bg = "bg"
  end

  let initialize context =
    let sw, sh = novel_game.width, novel_game.height in
    let bg =
      let open Entity in
      let pos = 0, 0 in
      let size = sw, sh in
      SingleImage.create Id.bg ResourceLabels.bg ~pos ~size
    in
    let open World in
    spawn [bg]
    >> MessageWindow.initialize context
    >> put_text "日本語のメッセージを正しくパースできるでしょうか？"

  let handle_event _context ev =
    let open World in
    match ev with
    | Event.MouseDown _ | Event.KeyDown {key_code = 32} ->
      clear_text ()
    | _ -> return ()

  let update context =
    let open World in
    MessageWindow.update context
    >> World.dbg_show_renderables

end

let _ =
  Game.add_scene novel_game "brand_logo" (module BrandLogo);
  Game.add_scene novel_game "title" (module GameTitle);
  start novel_game "brand_logo"