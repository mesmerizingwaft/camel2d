open Camel2d

let chohan = Game.create ()

module Literal = struct
  let cho = "\u{4e01}"
  let han = "\u{534a}"
  let speech = "\u{3055}\u{3042}\u{3055}\u{3042}\u{5f35}\u{3063}\u{305f}\u{5f35}\u{3063}\u{305f}\u{4e01}\u{65b9}\u{306a}\u{3044}\u{304b}\u{ff1f}\u{534a}\u{65b9}\u{306a}\u{3044}\u{304b}\u{ff1f}"
end

module GameMain : Scene = struct
  (* Entities *)
  let bg = Entity.create_from_image "bg" "bg" ~pos:(0, 0) ~size:(chohan.width, chohan.height)
  let fg =
    let (w, h) = (int_of_float (0.75 *. float_of_int chohan.height), chohan.height) in
    Entity.create_from_image "fg" "fg" ~pos:((chohan.width - w) / 2, 0) ~size:(w, h)
  let button_cho = Entity.create_from_text "button_cho" Literal.cho ~font_face:(Some "tamanegi") ~outline:(`Edging (0, 0, 0)) ~pt:100 ~pos:((chohan.width / 3 - 50), (chohan.height - 100)/2) ~size:(100, 100)
  let button_cho_on_mousehover = Entity.create_from_text "button_cho" Literal.cho ~color:(`RGBA (255, 200, 200, 1.)) ~font_face:(Some "tamanegi")  ~outline:(`Edging (0, 0, 0)) ~pt:100 ~pos:((chohan.width / 3 - 50), (chohan.height - 100)/2) ~size:(100, 100)
  let button_han = Entity.create_from_text "button_han" Literal.han ~font_face:(Some "tamanegi") ~outline:(`Edging (0, 0, 0)) ~pt:100 ~pos:((chohan.width / 3 * 2 - 50), (chohan.height - 100)/2) ~size:(100, 100)
  let button_han_on_mousehover = Entity.create_from_text "button_han" Literal.han ~color:(`RGBA (255, 200, 200, 1.)) ~font_face:(Some "tamanegi") ~outline:(`Edging (0, 0, 0)) ~pt:100 ~pos:((chohan.width / 3 * 2 - 50), (chohan.height - 100)/2) ~size:(100, 100)
  let speech = Entity.create_from_text "speech" Literal.speech ~font_face:(Some "tamanegi") ~outline:(`Edging (0, 0, 0)) ~pt:25 ~pos:((chohan.width - 25 * 21) / 2, 20) ~size:(100, 100)
  let create_dice id number ~pt ~idx =
    let text = [|"\u{2680}"; "\u{2681}"; "\u{2682}"; "\u{2683}"; "\u{2684}"; "\u{2685}"|].(number - 1) in
    Entity.create_from_text id text ~pt ~pos:(chohan.width / 3 * idx - pt / 2, (chohan.height - pt)/2) ~size:(pt, pt)
  let label_win = Entity.create_from_text "label_win" "\u{52dd}" ~font_face:(Some "tamanegi") ~outline:(`Edging (0, 0, 0)) ~pt:200 ~pos:((chohan.width - 200) / 2, (chohan.height - 200) / 2) ~size:(200, 200)
  let label_lose = Entity.create_from_text "label_lose" "\u{8ca0}" ~font_face:(Some "tamanegi") ~outline:(`Edging (0, 0, 0)) ~pt:200 ~pos:((chohan.width - 200) / 2, (chohan.height - 200) / 2) ~size:(200, 200)

  (* Logic *)
  type hand = Cho | Han
  type phase =
    | Init
    | DiceRolling of hand * int
    | End of hand * int * int
  let phase = ref Init
  let roll () = (Random.int 6) + 1
  let roll_twice () = (roll (), roll ())

  (* event handler *)
  let event_handler_init ev entities =
    match ev with
      | Event.MouseMove {x; y} ->
        let open Entity in
        entities
        |> (update_when
            (has_id "button_cho")
            (fun entity ->
              if is_in x y entity
              then button_cho_on_mousehover
              else button_cho))
        |> (update_when
            (has_id "button_han")
            (fun entity ->
              if is_in x y entity
              then button_han_on_mousehover
              else button_han))
        |> update_entities
      | Event.MouseUp {x; y; _} ->
        let open Entity in
        if List.exists (has_id "button_cho" &&& is_in x y) entities
        then phase := DiceRolling (Cho, 0);
        if List.exists (has_id "button_han" &&& is_in x y) entities
        then phase := DiceRolling (Han, 0);
        update_entities entities
      | _ -> update_entities entities

  let event_handler_end ev entities =
    match ev with
      | Event.MouseUp _ ->
        print_endline "load_new_scene";
        load_new_scene "main"
      | _ -> update_entities entities

  let event_handler ev entities =
    (match !phase with
      | Init -> event_handler_init
      | DiceRolling _ -> EventHandler.init
      | End _ -> event_handler_end) ev entities

  (* arbitrator *)
  let arbitrator entities =
    match !phase with
      | Init -> update_entities entities
      | DiceRolling (hand, counter) ->
        let a, b = roll_twice () in
        let dice_l = create_dice "dice_l" a ~pt:100 ~idx:1 in
        let dice_r = create_dice "dice_r" b ~pt:100 ~idx:2 in
        if counter <= 60
        then begin
          phase := DiceRolling (hand, counter + 1);
          update_entities [bg; fg; dice_l; dice_r]
        end
        else begin
          phase := End (hand, a, b);
          update_entities [bg; fg; dice_l; dice_r]
        end
      | End (hand, a, b) ->
        let dice_l = create_dice "dice_l" a ~pt:100 ~idx:1 in
        let dice_r = create_dice "dice_r" b ~pt:100 ~idx:2 in
        let entities = [bg; fg; dice_l; dice_r] in
        if hand = Cho && (a + b) mod 2 = 0
          || hand = Han && (a + b) mod 2 <> 0
        then update_entities (entities @ [label_win])
        else update_entities (entities @ [label_lose])

  let load_resources () =
    let img_root = "/samples/chohan/static/imgs/" in
    ResourceUtils.load_imgs img_root [
      ("bg", "bg.jpg");
      ("fg", "tsubofurishi.png")
    ]

  let start _ =
    Random.self_init ();
    phase := Init;
    let entities = [ bg; fg; speech; button_cho; button_han ] in
    entities, arbitrator, event_handler
end

let _ =
  Game.add_scene chohan "main" (module GameMain);
  start chohan "main" 
