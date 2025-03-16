open Camel2d

let _ = Random.self_init ()
let popping_alfie = Game.create ()

module Literal = struct
  type t = {
    tweet_body: string;
    url: string;
    label_tweet: string;
    label_retry: string;
  } [@@deriving yojson]

  let literals  = Result.get_ok @@ of_yojson @@ Yojson.Safe.from_string [%blob "literals.json"]
end

let literals = Literal.literals

module GameTitle : Scene.T = struct
  module ResourceLabels = struct
    open Resource
    let bgm = gen_label ()
    let bg = gen_label ()
  end

  let load_resources =
    let open Resource in
    let open ResourceLabels in
    set_audio_root "/samples/popping_alfie/static/audio/"
    >> set_audio_mode BGM
    >> load_audio bgm "bgm.mp3"
    >> set_image_root "/samples/popping_alfie/static/imgs/"
    >> load_image bg "bg.jpg"

  module Id = struct
    let bg = "bg"
    let title_logo = "title_logo"
    let title_inst = "title_inst"
  end

  let initialize _context =
    let sw, sh = popping_alfie.width, popping_alfie.height in
    let bg =
      let open Entity in
      let pos = 0, 0 in
      let size = sw, sh in
      SingleImage.create Id.bg ResourceLabels.bg ~pos ~size
    in
    let title_logo =
      let open Entity in
      let style =
        let pt = 50 in
        let font_face = Some "Mochiy Pop One" in
        let color = TextLabel.RGBA (255, 255, 255, 1.) in
        let outline = TextLabel.Edging (RGBA (0, 0, 0, 1.)) in
        TextLabel.create_style ~font_face ~color ~outline pt
      in
      let pos = sw / 2, sh / 2 - 100 in
      TextLabel.create ~style ~pos ~base_horizontal:BHCenter Id.title_logo "PoppingAlfie"
    in
    let title_inst =
      let open Entity in 
      let open TextLabel in
      let style =
        let pt = 30 in
        let font_face = Some "Mochiy Pop One" in
        let color = RGBA (0, 0, 0, 1.) in
        create_style ~font_face ~color pt
      in
      let pos = sw / 2, sh / 2 in
      create ~style ~pos ~base_horizontal:BHCenter Id.title_inst "click to start"
    in
    let open World in
    play_audio ResourceLabels.bgm
    >> spawn [bg; title_logo; title_inst]

  let handle_event _context ev =
    let open World in
    match ev with
      | Event.MouseUp _ ->
        start_scene "main"
      | _ -> return ()

  let counter = ref 0

  let flip_visibility =
    let open World in
    let is_target = Condition.has_id Id.title_inst in
    let* is_visible = exists Condition.(is_target &&& visible) in
    if is_visible then update_when is_target Updator.hide
    else update_when is_target Updator.show

  let update _context =
    let open World in
    let* cnt = use_ref counter in
    (if cnt mod 60 = 0
    then flip_visibility
    else return ())
    >> put_ref counter ((cnt + 1) mod Int.max_int)
end

module GameMain : Scene.T = struct
  let ground_height = popping_alfie.height - 100
  let init_alfie, alfie_updator, alfie_h = Alfie.create ground_height
  let gen_chamomile, chamomile_updator = Chamomile.create ground_height 100 popping_alfie.width
  let gen_strawberry, strawberry_updator = Strawberry.create ground_height 100 popping_alfie.width
  let score, init_score_label, score_label_updator = Score.create ()
  let gravity = 1
  let alfie's_speed = ref 0
  let jump_power = ref 0
  let jump_max = 25

  let update_physics =
    let open World in
    let* h = use_ref alfie_h in
    let* jp = use_ref jump_power in
    if jp > 0 && jump_max > jp then jump_power := (jp + 1);
    alfie's_speed := if h > 0 then !alfie's_speed - gravity else Int.max 0 !alfie's_speed;
    alfie_h := Int.max 0 (h + !alfie's_speed);
    return ()

  let gen_item_at_random =
    let open World in
    let* sc = use_ref score in
    let strawberry_rate = Float.min 0.9 (0.1 *. ((float_of_int sc) /. 1.)) in
    if Random.float 1. <= 0.01
    then begin
      if Random.float 1. <= strawberry_rate then
        gen_strawberry ()
      else
        gen_chamomile ()
    end
    else return ()

  let update_score context =
    let open World in
    let is_alfie = Condition.(has_id Alfie.Id.alfie_in_air ||| has_id Alfie.Id.alfie_steady) in
    let* alfie = find is_alfie in
    let* num_of_chamomile = num_of Condition.(has_id Chamomile.Id.chamomile) in
    remove_when Condition.(has_id Chamomile.Id.chamomile &&& check_collision_with alfie)
    >> let* num_of_chamomile' = num_of Condition.(has_id Chamomile.Id.chamomile) in
    score := !score + (num_of_chamomile - num_of_chamomile');
    score_label_updator context

  let check_gameover =
    let open World in
    let is_alfie = Condition.(has_id Alfie.Id.alfie_in_air ||| has_id Alfie.Id.alfie_steady) in
    let* alfie = find is_alfie in
    let* hit = exists Condition.(has_id Strawberry.Id.strawberry &&& check_collision_with alfie) in
    if hit then start_scene "gameover" else return ()

  module ResourceLabels = struct
    open Resource
    let bg = gen_label()
  end

  let load_resources =
    let open Resource in
    let open ResourceLabels in
    set_image_root "/samples/popping_alfie/static/imgs/"
    >> load_image bg "bg.jpg"
    >> Alfie.load_resources ()
    >> Chamomile.load_resources ()
    >> Strawberry.load_resources ()
  
  module Id = struct
    let bg = "bg"
  end

  let initialize context =
    let sw, sh = popping_alfie.width, popping_alfie.height in
    let open World in
    let bg =
      let open Entity in
      let pos = 0, 0 in
      let size = sw, sh in
      SingleImage.create Id.bg ResourceLabels.bg ~pos ~size
    in
    spawn [bg]
    >> init_alfie context
    >> init_score_label context

  let handle_event _context ev =
    let open World in
    let* jp = use_ref jump_power in
    match ev with
      | Event.MouseDown _ | Event.KeyDown {key_code = 32} ->
        if jp <= 0 then jump_power := 1;
        return ()
      | Event.MouseUp _ | Event.KeyUp {key_code = 32} ->
        alfie's_speed := jp;
        jump_power := 0;
        return ()
      | _ -> return ()

  let update context =
    let open World in
    update_physics
    >> alfie_updator
    >> gen_item_at_random
    >> chamomile_updator
    >> strawberry_updator
    >> update_score context
    >> check_gameover
end

module GameOver : Scene.T = struct

  module Id = struct
    let btn_tweet = "btn_tweet"
    let btn_retry = "btn_retry"
    let label_gameover = "label_gameover"
  end

  let load_resources = Resource.return ()

  let initialize _context =
    let open World in
    let open Entity in
    let cx, cy = popping_alfie.width / 2, popping_alfie.height / 2 in
    let label_gameover =
      let open TextLabel in
      let style =
        let pt = 20 in
        let font_face = Some "Mochiy Pop One" in
        let color = TextLabel.RGBA (255, 255, 255, 1.) in
        create_style ~font_face ~color pt
      in
      create
        ~style
        ~pos:(cx, cy - 50)
        ~base_horizontal: BHCenter
        Id.label_gameover
        "GAMEOVER"
    in
    let style_btns =
      let open TextLabel in
      create_style 10
    in
    let btn_tweet =
      let open TextLabel in
      create
        ~style:style_btns
        ~pos:(cx, cy)
        ~base_horizontal:BHCenter
        Id.btn_tweet
        literals.label_tweet in
    let btn_retry =
      let open TextLabel in
      create
        ~style: style_btns
        ~pos:(cx, cy + 20)
        ~base_horizontal: BHCenter
        Id.btn_retry
        literals.label_retry
    in
    spawn [label_gameover; btn_tweet; btn_retry]

  let handle_event _context ev =
    let open World in
    match ev with
      | Event.MouseUp {x; y; _} ->
        let* score' = use_ref Score.score in
        let* btn_retry_clicked = exists Condition.(has_id Id.btn_retry &&& is_in x y) in
        let* btn_tweet_clicked = exists Condition.(has_id Id.btn_tweet &&& is_in x y) in
        if btn_tweet_clicked then begin
          SnsUtils.tweet
            (literals.tweet_body ^ string_of_int score')
            ~url:(Some literals.url);
          return ()
        end else if btn_retry_clicked then begin
          Score.reset ();
          start_scene "main"
        end else return ()
      | _ -> return ()

  let update _context = World.return ()
end


let _ =
  Game.add_scene popping_alfie "title" (module GameTitle);
  Game.add_scene popping_alfie "main" (module GameMain);
  Game.add_scene popping_alfie "gameover" (module GameOver);
  start popping_alfie "title"


