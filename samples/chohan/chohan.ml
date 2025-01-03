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

  (* arbitrator *)
  let arbitrator entities =
    match !phase with
      | Init ->
        let entities = ref entities in
        let rec event_loop () =
          match Event.take () with
            | None -> ()
            | Some (Event.MouseMove {x; y}) ->
              entities := List.map (fun entity ->
                match entity with
                  | Entity.E {id; _} when id = "button_cho" ->
                    if Entity.is_in entity x y
                    then button_cho_on_mousehover
                    else button_cho
                  | Entity.E {id; _} when id = "button_han" ->
                    if Entity.is_in entity x y
                    then button_han_on_mousehover
                    else button_han
                  | _ -> entity
              ) !entities
            | Some (Event.MouseUp {x; y; _}) ->
              List.iter (fun entity ->
                match entity with
                  | Entity.E {id; _} when Entity.is_in entity x y ->
                    if id = "button_cho" then phase := DiceRolling (Cho, 0)
                    else if id = "button_han" then phase := DiceRolling (Han, 0)
                    else ()
                  | _ -> ()
              ) !entities
            | _ -> event_loop ()
        in
        event_loop ();
        Arbitrator.Update !entities
      | DiceRolling (hand, counter) ->
        let a, b = roll_twice () in
        let dice_l = create_dice "dice_l" a ~pt:100 ~idx:1 in
        let dice_r = create_dice "dice_r" b ~pt:100 ~idx:2 in
        if counter <= 60
        then begin
          phase := DiceRolling (hand, counter + 1);
          Arbitrator.Update [bg; fg; dice_l; dice_r]
        end
        else begin
          phase := End (hand, a, b);
          Arbitrator.Update [bg; fg; dice_l; dice_r]
        end
      | End (hand, a, b) ->
        let rec is_clicked () =
          match Event.take () with
            | None -> false
            | Some (Event.MouseUp _) -> true
            | _ -> is_clicked ()
        in
        if is_clicked () then Arbitrator.LoadScene "main"
        else
          let dice_l = create_dice "dice_l" a ~pt:100 ~idx:1 in
          let dice_r = create_dice "dice_r" b ~pt:100 ~idx:2 in
          let entities = [bg; fg; dice_l; dice_r] in
          if hand = Cho && (a + b) mod 2 = 0
            || hand = Han && (a + b) mod 2 <> 0
          then Arbitrator.Update (entities @ [label_win])
          else Arbitrator.Update (entities @ [label_lose])

  let load_resources () =
    let open Promise in
    Resource.load_img "/samples/chohan/static/imgs/bg.jpg" >>= fun bg ->
    Resource.load_img "/samples/chohan/static/imgs/tsubofurishi.png" >>= fun fg ->
    let bucket = Resource.create_bucket () in
    Hashtbl.add bucket "bg" bg;
    Hashtbl.add bucket "fg" fg;
    return bucket

  let start () =
    Random.self_init ();
    phase := Init;
    let entities = [ bg; fg; speech; button_cho; button_han ] in
    entities, arbitrator
end

let _ =
  Game.add_scene chohan "main" (module GameMain);
  start chohan "main" 
