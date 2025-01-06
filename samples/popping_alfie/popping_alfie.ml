open Camel2d

let img_root = "./static/imgs/"
let popping_alfie = Game.create ()

let score = ref 0

module GameTitle: Scene = struct
  (* Entities *)
  let bg = Entity.create_from_image "bg" "bg" ~pos:(0, 0) ~size:(popping_alfie.width, popping_alfie.height)
  let title_logo =
    let pt = 80 in
    let pos = popping_alfie.width / 2, popping_alfie.height / 2 - 100 in
    Entity.create_from_text
      "title_logo"
      "Popping Alfie"
      ~font_face:(Some "Mochiy Pop One")
      ~pt
      ~color:(`RGBA (255, 255, 255, 1.))
      ~outline:(`Edging (0, 0, 0))
      ~pos
      ~size:(100, 100)
      ~base_horizontal:`Center
  let title_inst =
    let pt = 30 in
    let pos = popping_alfie.width / 2, popping_alfie.height / 2 in
    Entity.create_from_text
      "title_inst"
      "click to start"
      ~font_face:(Some "Mochiy Pop One")
      ~pt
      ~color:(`RGBA(0, 0, 0, 1.))
      ~pos
      ~size:(10, 10)
      ~base_horizontal:`Center

  (* Logic *)
  let counter = ref 0
  let show_inst = ref true

  (* event handler *)
  let evnet_handler ev entities =
    match ev with
      | Event.MouseUp _ ->
        load_new_scene "main"
      | _ -> update_entities entities

  (* Arbitrator *)
  let arbitrator _entities =
    counter := (!counter + 1) mod Int.max_int;
    if !counter mod 60 = 0 then show_inst := not !show_inst;
    if !show_inst
    then update_entities [bg; title_logo; title_inst]
    else update_entities [bg; title_logo]

  let load_resources () =
    let open Promise in
    Resource.load_img (img_root ^ "bg.jpg") >>= fun bg ->
    let bucket = Resource.create_bucket () in
    Hashtbl.add bucket "bg" bg;
    return bucket

  let start _ =
    counter := 0;
    show_inst := true;
    [bg; title_logo; title_inst], arbitrator, evnet_handler
end

module GameMain: Scene = struct
  let ground_height = popping_alfie.height - 100
  let bg = Entity.create_from_image "bg" "bg" ~pos:(0, 0) ~size:(popping_alfie.width, popping_alfie.height)
  let alfie1 = Entity.create_from_image "alfie" "alfie1" ~pos:(10, ground_height) ~size:(80, 50)
  let alfie2 = Entity.create_from_image "alfie" "alfie2" ~pos:(10, ground_height) ~size:(90, 50)
  let gen_chamomile () =
    let h = ground_height - Random.int (popping_alfie.height - ground_height)  in
    Entity.create_from_image "chamomile" "chamomile" ~pos:(popping_alfie.width, h) ~size:(30, 30)
  let gen_strawberry () =
    let h = ground_height - Random.int (popping_alfie.height - ground_height)  in
    Entity.create_from_image "strawberry" "strawberry" ~pos:(popping_alfie.width, h) ~size:(30, 30)
  let gen_score score =
    let text = Printf.sprintf "score: %d" score in
    Entity.create_from_text "score" text ~color:(`RGBA (255, 255, 255, 1.)) ~outline:(`Edging (0, 0, 0)) ~pt:50 ~pos:(10, 10) ~size:(50 * 9, 50)

  (* Logic *)
  let alfie's_hight = ref 0
  let alfie's_speed = ref 0
  let gravity = 1
  let jump_power = ref 0
  let jump_max = 25

  let update_physics () =
    if !jump_power > 0 then jump_power := Int.min jump_max (!jump_power + 1);
    alfie's_speed := if !alfie's_hight > 0 then !alfie's_speed - gravity else Int.max 0 !alfie's_speed;
    alfie's_hight := Int.max 0 (!alfie's_hight + !alfie's_speed)

  (* event handler *)
  let event_handler ev entities =
    if !alfie's_hight = 0 then begin
      match ev with
        | Event.MouseDown _ | Event.KeyDown {key_code = 32} ->
          jump_power := 1
        | Event.MouseUp _ | Event.KeyUp {key_code = 32} ->
          alfie's_speed := !jump_power;
          jump_power := 0
        | _ -> ()
    end;
    update_entities entities

  (* Arbitrator *)
  let gen_item_at_random entities =
    let strawberry_rate = Float.min 0.9 (0.1 *. ((float_of_int !score) /. 1.)) in
    print_endline @@ Printf.sprintf "rate: %f" strawberry_rate;
    if Random.float 1. <= 0.01
    then begin
      if Random.float 1. <= strawberry_rate then
        entities @ [gen_strawberry ()]
      else
        entities @ [gen_chamomile ()]
    end
    else entities

  let update_alfie's_y entities =
    let open Entity in
    let new_y = (ground_height - !alfie's_hight) in
    entities
    |> update_when (has_id "alfie")
      (fun (E entity) -> E { entity with y = new_y})

  let update_items entities =
    let open Entity in
    entities
    |> update_when ((has_id "chamomile") ||| (has_id "strawberry"))
      (fun (E entity) -> E { entity with x = entity.x - 3 })

  let update_alfie's_texture entities =
    let open Entity in
    entities
    |> update_when (has_id "alfie") (fun (E {y; _}) ->
      let E alfie1 = alfie1 in
      let E alfie2 = alfie2 in
      if !alfie's_hight > 0 then E {alfie2 with y} else E {alfie1 with y}
    )

  let remove_expired_items entities =
    let open Entity in
    let alfie = List.find (has_id "alfie") entities in
    entities
    |> remove_when (
      ((has_id "chamomile") ||| (has_id "strawberry"))
      &&& ((x_is_smaller_than 0) ||| (check_collision alfie)))

  let update_score entities =
    let open Entity in
    let alfie = List.find (has_id "alfie") entities in
    score := !score + (count_when ((has_id "chamomile") &&& (check_collision alfie)) entities);
    entities
    |> update_when (has_id "score") (fun _ -> gen_score !score)

  let check_gameover entities =
    let open Entity in
    let alfie = List.find (has_id "alfie") entities in
    List.exists (has_id "strawberry" &&& check_collision alfie) entities

  let arbitrator entities =
    update_physics ();
    let entities = entities
      |> update_alfie's_y
      |> update_items
      |> update_score
    in
    if check_gameover entities then load_new_scene "gameover"
    else
      entities
      |> remove_expired_items
      |> update_alfie's_texture
      |> gen_item_at_random
      |> update_entities

  let load_resources () =
    ResourceUtils.load_imgs img_root [
      ("bg", "bg.jpg");
      ("alfie1", "alfie1.png");
      ("alfie2", "alfie2.png");
      ("chamomile", "flower_chamomile.png");
      ("strawberry", "fruit_strawberry.png")
    ]

  let start _ =
    Random.self_init ();
    score := 0;
    alfie's_hight := 0;
    alfie's_speed := 0;
    jump_power := 0;
    [bg; alfie1; alfie2; gen_score 0], arbitrator, event_handler
end

module GameOver : Scene = struct

  module Literal = struct
    let label_tweet = "\u{7d50}\u{679c}\u{3092}tweet\u{3059}\u{308b}"
    let label_retry = "\u{30ea}\u{30c8}\u{30e9}\u{30a4}?"
    let tweet_body = "PoppingAlfie\u{3067}\u{904a}\u{3093}\u{3060}\u{3088}\u{ff01}\r\nScore:"
  end

    let load_resources () =
      ResourceUtils.load_imgs img_root []

  let event_handler ev entities =
    let btn_tweet = List.find Entity.(has_id "btn_tweet") entities in
    let btn_retry = List.find Entity.(has_id "btn_retry") entities in
    match ev with
      | Event.MouseUp {x; y; _} when Entity.is_in x y btn_tweet ->
        SNSUtils.tweet
          (Literal.tweet_body ^ string_of_int !score)
          ~url:(Some "https://www.waft-games.com/popping_alfie");
        update_entities entities
      | Event.MouseUp {x; y; _} when Entity.is_in x y btn_retry ->
        load_new_scene "main"
      | _ ->
        update_entities entities

  let arbitrator = Arbitrator.init

  let start context =
    let cx, cy = popping_alfie.width / 2, popping_alfie.height / 2 in
    let label_gameover =
      let open Entity.TextLabel in
      let style = create_style
        ~font_face:(Some "Mochiy Pop One") 20
      in        
      create
        ~context
        ~style
        ~pos:(cx, cy - 50)
        ~base_horizontal:BHCenter
        "label_gameover"
        "GAMEOVER"
      in
    let style_btns = Entity.TextLabel.create_style 10 in
    let btn_tweet =
      let open Entity.TextLabel in
      create
        ~context
        ~style:style_btns
        ~pos:(cx, cy)
        ~base_horizontal:BHCenter
        "btn_tweet"
        Literal.label_tweet in
    let btn_retry =
      let open Entity.TextLabel in
      create
        ~context
        ~style: style_btns
        ~pos:(cx, cy + 20)
        ~base_horizontal: BHCenter
        "btn_retry"
        Literal.label_retry
    in
    Random.self_init ();
    [label_gameover; btn_tweet; btn_retry], arbitrator, event_handler

end

let _ =
  Game.add_scene popping_alfie "title" (module GameTitle);
  Game.add_scene popping_alfie "main" (module GameMain);
  Game.add_scene popping_alfie "gameover" (module GameOver);
  start popping_alfie "title"
