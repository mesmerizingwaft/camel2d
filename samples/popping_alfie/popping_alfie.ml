open Camel2d

let img_root = "/samples/popping_alfie/static/imgs/"
let popping_alfie = Game.create ()

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

  let start () =
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
    let h = Random.int (popping_alfie.height) in
    Entity.create_from_image "chamomile" "chamomile" ~pos:(popping_alfie.width, h) ~size:(50, 50)
  let gen_score score =
    let text = Printf.sprintf "score: %d" score in
    Entity.create_from_text "score" text ~color:(`RGBA (255, 255, 255, 1.)) ~outline:(`Edging (0, 0, 0)) ~pt:50 ~pos:(10, 10) ~size:(50 * 9, 50)

  (* Logic *)
  let score = ref 0
  let alfie's_hight = ref 0
  let alfie's_speed = ref 0
  let gravity = 1
  let jump_power = ref 0
  let jump_max = 20

  let update_alfie's_y new_y =
    let E {y; _} = alfie1 in y := new_y;
    let E {y; _} = alfie2 in y := new_y

  let update_physics () =
    if !jump_power > 0 then jump_power := Int.min jump_max (!jump_power + 1);
    alfie's_speed := if !alfie's_hight > 0 then !alfie's_speed - gravity else Int.max 0 !alfie's_speed;
    alfie's_hight := Int.max 0 (!alfie's_hight + !alfie's_speed)

  (* event handler *)
  let event_handler ev entities =
    if !alfie's_hight = 0 then begin
      match ev with
        | Event.MouseDown _ ->
          jump_power := 1
        | Event.MouseUp _ ->
          alfie's_speed := !jump_power;
          jump_power := 0
        | _ -> ()
    end;
    update_entities entities

  (* Arbitrator *)
  let gen_item_at_random entities =
    if Random.float 1. <= 0.01
    then entities @ [gen_chamomile ()]
    else entities

  let update_score entities =
    let open Entity in
    entities
    |> update_when (has_id "score")
      (fun _ -> gen_score !score)

  let arbitrator entities =
    print_endline @@ Printf.sprintf "Score: %d" !score;
    update_physics ();
    update_alfie's_y (ground_height - !alfie's_hight);
    let entities = gen_item_at_random entities in
    List.iter (fun (Entity.E {id; x; _}) ->
      if id = "chamomile" then x := !x - 1
    ) entities;
    let entities =
      let open Entity in
      entities
      |> remove_when ((has_id "chamomile") &&& (x_is_smaller_than 0))
    in
    let entities =
      let open Entity in
      let entities' =
        remove_when ((has_id "chamomile") &&& (check_collision alfie1)) entities
      in
      score := !score + (List.length entities) - (List.length entities');
      entities'
    in
    let entities =
      let open Entity in
      entities
      |> update_when (has_id "alfie")
        (fun _ -> if !alfie's_hight > 0 then alfie2 else alfie1)
      |> update_score
    in
    update_entities entities

  let load_resources () =
    ResourceUtils.load_imgs img_root [
      ("bg", "bg.jpg");
      ("alfie1", "alfie1.png");
      ("alfie2", "alfie2.png");
      ("chamomile", "flower_chamomile.png")
    ]

  let start () =
    Random.self_init ();
    alfie's_hight := 0;
    alfie's_speed := 0;
    jump_power := 0;
    [bg; alfie1; alfie2; gen_score 0], arbitrator, event_handler
end

let _ =
  Game.add_scene popping_alfie "title" (module GameTitle);
  Game.add_scene popping_alfie "main" (module GameMain);
  start popping_alfie "title"
