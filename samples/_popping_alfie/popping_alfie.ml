open Camel2d

let img_root = "./static/imgs/"
let popping_alfie = Game.create ()

let score = ref 0

module Id = struct
  let bg = "bg"
  let title_logo = "title_logo"
  let title_inst = "title_inst"
  let alfie1 = "alfie1"
  let alfie2 = "alfie2"
  let chamomile = "chamonile"
  let strawberry = "strawberry"
  let score = "score"
  let btn_tweet = "btn_tweet"
  let btn_retry = "btn_retry"
end

module Literal = struct
  let title_logo = "Popping Alfie"
  let title_inst = "click to start"
  let label_tweet = "\u{7d50}\u{679c}\u{3092}tweet\u{3059}\u{308b}"
  let label_retry = "\u{30ea}\u{30c8}\u{30e9}\u{30a4}?"
  let tweet_body = "PoppingAlfie\u{3067}\u{904a}\u{3093}\u{3060}\u{3088}\u{ff01}\r\nScore:"
end

let bg = let open Renderable in
  let pos = (0, 0) in
  let size = (popping_alfie.width, popping_alfie.height) in
  SingleImage.create ~pos ~size Id.bg "bg"

module GameTitle: Scene = struct
  (* Logic *)
  let counter = ref 0
  let show_inst = ref true

  (* event handler *)
  let evnet_handler context ev renderables audios =
    let update_renderables renderables = update_scene renderables audios in
    match ev with
      | Event.MouseUp _ ->
        Audio.resume context;
        load_new_scene "main"
      | _ -> update_renderables renderables

  (* Arbitrator *)
  let arbitrator bg title_logo title_inst _context _renderables audios =
    let update_renderables renderables = update_scene renderables audios in
    counter := (!counter + 1) mod Int.max_int;
    if !counter mod 60 = 0 then show_inst := not !show_inst;
    if !show_inst
    then update_renderables [bg; title_logo; title_inst]
    else update_renderables [bg; title_logo]

  let load_resources _context =
    let open Promise in
    Resource.load_img (img_root ^ "bg.jpg") >>= fun bg ->
    let bucket = Resource.create_bucket () in
    Hashtbl.add bucket "bg" bg;
    let audio_bucket = Audio.create_bucket () in
    return (bucket, audio_bucket)

  let start context =
    let title_logo = let open Renderable in let open TextLabel in
      let style =
        let pt = 80 in
        let font_face = Some "Mochiy Pop One" in
        let color = RGBA (255, 255, 255, 1.) in
        let outline = Edging (RGBA (0, 0, 0, 1.)) in
        create_style ~font_face ~color ~outline pt
      in
      let pos = popping_alfie.width / 2, popping_alfie.height / 2 - 100 in
      create ~context ~style ~pos ~base_horizontal:BHCenter Id.title_logo Literal.title_logo
    in
    let title_inst = let open Renderable in let open TextLabel in
      let style =
        let pt = 30 in
        let font_face = Some "Mochiy Pop One" in
        let color = RGBA (0, 0, 0, 1.) in
        create_style ~font_face ~color pt
      in
      let pos = popping_alfie.width / 2, popping_alfie.height / 2 in
      create ~context ~style ~pos ~base_horizontal:BHCenter Id.title_inst Literal.title_inst
    in
    let arbitrator = arbitrator bg title_logo title_inst in
    counter := 0;
    show_inst := true;
    [bg; title_logo; title_inst], [], arbitrator, evnet_handler
end

module GameMain: Scene = struct
  let ground_height = popping_alfie.height - 100
  let alfie1, alfie2 =
    let open Renderable in
    let open SingleImage in
    let pos = (10, ground_height) in
    create Id.alfie1 "alfie1" ~pos ~size:(80, 50),
    create Id.alfie2 "alfie2" ~pos ~size:(90, 50)
  let gen_chamomile () =
    let open Renderable in 
    let open SingleImage in
    let h = ground_height - Random.int (popping_alfie.height - ground_height)  in
    create Id.chamomile "chamomile" ~pos:(popping_alfie.width, h) ~size:(30, 30)
  let gen_strawberry () =
    let open Renderable in
    let open SingleImage in
    let h = ground_height - Random.int (popping_alfie.height - ground_height)  in
    create Id.strawberry "strawberry" ~pos:(popping_alfie.width, h) ~size:(30, 30)
  let gen_score context score =
    let open Renderable in
    let open TextLabel in
    let style =
      let color = RGBA (255, 255, 255, 1.) in
      let outline = Edging (RGBA (0, 0, 0, 1.)) in
      let pt = 50 in
      create_style ~color ~outline pt
    in
    let pos = (10, 10) in
    let text = Printf.sprintf "score: %d" score in
    create ~context ~style ~pos Id.score text

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
  let event_handler _ ev renderables audios =
    let update_renderables renderables = update_scene renderables audios in
    if !alfie's_hight = 0 then begin
      match ev with
        | Event.MouseDown _ | Event.KeyDown {key_code = 32} ->
          jump_power := 1
        | Event.MouseUp _ | Event.KeyUp {key_code = 32} ->
          alfie's_speed := !jump_power;
          jump_power := 0
        | _ -> ()
    end;
    update_renderables renderables

  (* Arbitrator *)
  let gen_item_at_random renderables =
    let strawberry_rate = Float.min 0.9 (0.1 *. ((float_of_int !score) /. 1.)) in
    print_endline @@ Printf.sprintf "rate: %f" strawberry_rate;
    if Random.float 1. <= 0.01
    then begin
      if Random.float 1. <= strawberry_rate then
        renderables @ [gen_strawberry ()]
      else
        renderables @ [gen_chamomile ()]
    end
    else renderables

  let update_alfie's_y renderables =
    let open RenderableUtils in
    let new_y = (ground_height - !alfie's_hight) in
    renderables
    |> update_when (has_id Id.alfie1 ||| has_id Id.alfie2) (update_y new_y)

  let update_items renderables =
    let open Renderable in
    let open RenderableUtils in
    let incr_x (Renderable r) = Renderable {r with x = r.x + 3} in
    renderables
    |> update_when ((has_id Id.chamomile) ||| (has_id Id.strawberry)) incr_x

  let update_alfie's_texture renderables =
    let open RenderableUtils in
    renderables
    |> update_when (has_id Id.alfie1) (fun r ->
      if !alfie's_hight > 0 then hide r else show r)
    |> update_when (has_id Id.alfie2) (fun r ->
      if !alfie's_hight > 0 then show r else hide r)

  let get_alfie renderables =
    let open RenderableUtils in
    let id = if !alfie's_hight > 0 then Id.alfie2 else Id.alfie1 in
    List.find (has_id id) renderables

  let remove_expired_items renderables =
    let open RenderableUtils in
    let alfie = get_alfie renderables in
    renderables
    |> remove_when (
      ((has_id Id.chamomile) ||| (has_id Id.strawberry))
      &&& ((x_is_smaller_than 0) ||| (check_collision alfie)))

  let update_score context renderables =
    let open RenderableUtils in
    let alfie = get_alfie renderables in
    score := !score + (count_when ((has_id Id.chamomile) &&& (check_collision alfie)) renderables);
    renderables
    |> update_when (has_id Id.score) (fun _ -> gen_score context !score)

  let check_gameover renderables =
    let open RenderableUtils in
    let alfie = get_alfie renderables in
    renderables
    |> List.exists (has_id Id.strawberry &&& check_collision alfie)

  let arbitrator context renderables audios =
    let update_renderables renderables = update_scene renderables audios in
    update_physics ();
    let renderables = renderables
      |> update_alfie's_y
      |> update_items
      |> update_score context
    in
    if check_gameover renderables then load_new_scene "gameover"
    else
      renderables
      |> remove_expired_items
      |> update_alfie's_texture
      |> gen_item_at_random
      |> update_renderables

  let load_resources _context =
    let open Promise in
    ResourceUtils.load_imgs img_root [
      ("bg", "bg.jpg");
      ("alfie1", "alfie1.png");
      ("alfie2", "alfie2.png");
      ("chamomile", "flower_chamomile.png");
      ("strawberry", "fruit_strawberry.png")
    ] >>= fun resource_bucket -> 
      let audio_bucket = Audio.create_bucket () in
      return (resource_bucket, audio_bucket)


  let start context =
    Random.self_init ();
    score := 0;
    alfie's_hight := 0;
    alfie's_speed := 0;
    jump_power := 0;
    [bg; alfie1; alfie2; gen_score context 0], [], arbitrator, event_handler
end

module GameOver : Scene = struct

  let load_resources _context =
    let open Promise in
    ResourceUtils.load_imgs img_root [] >>= fun resource_bucket -> 
    let audio_bucket = Audio.create_bucket () in
    return (resource_bucket, audio_bucket)

  let get_btn_tweet renderables =
    let open RenderableUtils in
    List.find (has_id Id.btn_tweet) renderables

  let get_btn_retry renderables =
    let open RenderableUtils in
    List.find (has_id Id.btn_retry) renderables

  let event_handler _ ev renderables audios =
    let open RenderableUtils in
    let update_renderables renderables = update_scene renderables audios in
    let btn_tweet = get_btn_tweet renderables in
    let btn_retry = get_btn_retry renderables in
    match ev with
      | Event.MouseUp {x; y; _} when is_in x y btn_tweet ->
        SNSUtils.tweet
          (Literal.tweet_body ^ string_of_int !score)
          ~url:(Some "https://www.waft-games.com/popping_alfie");
        update_renderables renderables
      | Event.MouseUp {x; y; _} when is_in x y btn_retry ->
        load_new_scene "main"
      | _ ->
        update_renderables renderables

  let arbitrator = Arbitrator.init

  let start context =
    let cx, cy = popping_alfie.width / 2, popping_alfie.height / 2 in
    let label_gameover =
      let open Renderable.TextLabel in
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
    let style_btns = Renderable.TextLabel.create_style 10 in
    let btn_tweet =
      let open Renderable.TextLabel in
      create
        ~context
        ~style:style_btns
        ~pos:(cx, cy)
        ~base_horizontal:BHCenter
        Id.btn_tweet
        Literal.label_tweet in
    let btn_retry =
      let open Renderable.TextLabel in
      create
        ~context
        ~style: style_btns
        ~pos:(cx, cy + 20)
        ~base_horizontal: BHCenter
        Id.btn_retry
        Literal.label_retry
    in
    Random.self_init ();
    [label_gameover; btn_tweet; btn_retry], [], arbitrator, event_handler

end

let _ =
  Game.add_scene popping_alfie "title" (module GameTitle);
  Game.add_scene popping_alfie "main" (module GameMain);
  Game.add_scene popping_alfie "gameover" (module GameOver);
  start popping_alfie "title"
