open Camel2d

let _ = Random.self_init ()
let chohan = Game.create ()

module Literal = struct
  type t = {
    cho: string;
    han: string;
    speech: string;
    dice_face: string array;
    win: string;
    lose: string;
    message: string;
  } [@@deriving yojson]
  let literals = Result.get_ok @@ of_yojson @@ Yojson.Safe.from_string [%blob "literals.json"]
end

let literals = Literal.literals

module Agreement = Templates.SimpleAgreement4BGM.Make(struct
  let game = chohan
  let message = literals.message
  let pt = 15
end)

module GameMain : Scene.T = struct
  module ResourceLabels = struct
    open Resource
    let bgm1 = gen_label ()
    let bgm2 = gen_label ()
    let bg = gen_label ()
    let fg = gen_label ()
  end

  let load_resources =
    let open Resource in
    let open ResourceLabels in
    set_audio_root "/samples/chohan/static/audio/"
    >> set_audio_mode BGM
    >> load_audio bgm1 "bgm1.mp3"
    >> load_audio bgm2 "bgm2.mp3"
    >> set_image_root "/samples/chohan/static/imgs/"
    >> load_image bg "bg.jpg"
    >> load_image fg "tsubofurishi.png"

  (* Entity IDs *)
  module Id = struct
    let bg = "bg"
    let fg = "fg"
    let button_cho = "button_cho"
    let button_cho_mousehover = "button_cho_mousehover"
    let button_han = "button_han"
    let button_han_mousehover = "button_han_mousehover"
    let win = "win"
    let lose = "lose"
    let dice_l = "dice_l"
    let dice_r = "dice_r"
    let speech = "speech"
  end

  (* Entity functions *)
  let create_dice context id number ~idx =
    let open Entity.Renderable in
    let open TextLabel in
    let pt = 100 in
    let style = create_style pt in
    let pos = (chohan.width / 3 * idx, (chohan.height - pt) / 2) in
    create ~context ~style ~pos ~base_horizontal: BHCenter id literals.dice_face.(number - 1)

  let create_label context label text =
    let open Entity.Renderable in
    let open TextLabel in
    let pt = 200 in
    let style =
      let font_face = Some "tamanegi" in
      let outline = Edging (RGBA (0, 0, 0, 1.)) in
      create_style ~font_face ~outline pt
    in
    let pos = (chohan.width / 2, (chohan.height - pt) / 2) in
    create ~context ~style ~pos ~base_horizontal:BHCenter label text

  (* Logic *)
  type hand = Cho | Han
  type phase =
    | Init
    | DiceRolling of hand * int
    | End of hand * int * int
  let phase = ref Init
  let roll () = (Random.int 6) + 1
  let roll_twice () = (roll (), roll ())
  let update_phase new_phase =
    phase := new_phase;
    match new_phase with
      | Init -> print_endline "new_phase=Init"
      | DiceRolling _ -> print_endline "new_phase=DiceRolling"
      | End _ -> print_endline "new_phase=End"
    

  let initialize context =
    let sw, sh = chohan.width, chohan.height in
    let bgm = Entity.Playable.(
      create "bgm" ResourceLabels.bgm1 |> set_to_play
    ) in
    let bg, fg =
      let open Entity.Renderable in
      let l_bg = ResourceLabels.bg in
      let l_fg = ResourceLabels.fg in
      let bg = SingleImage.create Id.bg l_bg ~pos:(0, 0) ~size:(sw, sh) in
      let fg = let (w, h) = (int_of_float (0.75 *. float_of_int sh), sh) in
        SingleImage.create Id.fg l_fg ~pos:((sw - w) / 2, 0) ~size:(w, h) in
      bg, fg
    in
    let button_cho, button_cho_on_mousehover =
    let open Entity.Renderable in
    let open TextLabel in
    let gen id color =
      let pt = 100 in
      let outline = Edging (RGBA (0, 0, 0, 1.)) in
      let font_face = Some "tamanegi" in
      let pos = ((sw / 3 - 50), (sh - 100)/2) in
      let style = create_style ~color ~outline ~font_face pt in
      create ~context ~style ~pos id literals.cho
    in
    gen Id.button_cho (RGBA (255, 255, 255, 1.)),
    gen Id.button_cho_mousehover (RGBA (255, 200, 200, 1.)) in
    let button_han, button_han_on_mousehover =
    let open Entity.Renderable in
    let open TextLabel in
    let gen id color =
      let pt = 100 in
      let outline = Edging (RGBA (0, 0, 0, 1.)) in
      let font_face = Some "tamanegi" in
      let pos = ((sw / 3 * 2 - 50), (sh - 100)/2) in
      let style = TextLabel.create_style ~color ~outline ~font_face pt in
      create ~context ~style ~pos id literals.han
    in
    gen Id.button_han (RGBA (255, 255, 255, 1.)),
    gen Id.button_han_mousehover (RGBA (255, 200, 200, 1.)) in
    let speech =
      let open Entity.Renderable in
      let open TextLabel in
      let style =
        let font_face = Some "tamanegi" in
        let outline = Edging (RGBA (0, 0, 0, 1.)) in
        create_style ~font_face ~outline 25
      in
      create ~context ~style ~pos:((sw - 25 * 21) / 2, 20) Id.speech literals.speech
    in
    let open World in
    let* _ = dbg_show_renderables in
    print_endline "initialize!!!!";
    update_phase Init;
    spawn_p [bgm]
    >> spawn_r [
      bg;
      fg;
      speech;
      button_cho; (RenderableUtils.hide button_cho_on_mousehover);
      button_han; (RenderableUtils.hide button_han_on_mousehover)
    ]
    >> let+ _ = dbg_show_renderables in
    print_endline "the end of init"

  let handle_event _context ev =
    let open World in
    let init_handler = function
      | Event.MouseMove {x; y} ->
        let is_button_default = Condition.((has_id_r Id.button_cho) ||| (has_id_r Id.button_han)) in
        let is_button_4_hover = Condition.((has_id_r Id.button_cho_mousehover) ||| (has_id_r Id.button_han_mousehover)) in
        let is_mouse_on = Condition.is_in x y in
        update_when is_button_default Updator.show
        >> update_when is_button_4_hover Updator.hide
        >> update_when Condition.(is_button_default &&& is_mouse_on) Updator.hide
        >> update_when Condition.(is_button_4_hover &&& is_mouse_on) Updator.show
      | Event.MouseUp {x; y; _} ->
        let* cho_clicked = exists Condition.(has_id_r Id.button_cho &&& is_in x y) in
        let* han_clicked = exists Condition.(has_id_r Id.button_han &&& is_in x y) in
        if cho_clicked then update_phase @@ DiceRolling(Cho, 0)
        else if han_clicked then update_phase @@ DiceRolling(Han, 0);
        if cho_clicked || han_clicked then
          let is_button = Condition.(
            any_of (List.map has_id_r Id.[button_cho; button_cho_mousehover; button_han; button_han_mousehover])
          ) in
          update_when is_button Updator.hide
        else
          return ()
      | _ -> return ()
    in
    let final_handler = function
      | Event.MouseUp _ -> start_scene "main"
      | _ -> return ()
    in
    let* _ = dbg_show_renderables in
    match !phase with
      | Init -> init_handler ev
      | DiceRolling _ -> return ()
      | End _ -> final_handler ev

  let update context =
    let open World in
    let* _ = dbg_show_renderables in
    match !phase with
      | Init -> return ()
      | DiceRolling (hand, counter) ->
        let a, b = roll_twice () in
        let dice_l = create_dice context Id.dice_l a ~idx:1 in
        let dice_r = create_dice context Id.dice_r b ~idx:2 in
        if counter <= 60 then update_phase @@ DiceRolling (hand, counter + 1)
        else update_phase @@ End (hand, a, b);
        let* dice_exists = exists Condition.(has_id_r Id.dice_l ||| has_id_r Id.dice_r) in
        if dice_exists
        then
          replace_by_id_r Id.dice_l dice_l
          >> replace_by_id_r Id.dice_r dice_r
        else
          spawn_r [dice_l; dice_r]
      | End (hand, a, b) ->
        let* results_shown = exists Condition.(has_id_r Id.win ||| has_id_r Id.lose) in
        if results_shown then return ()
        else
          let label_win = create_label context Id.win literals.win in
          let label_lose = create_label context Id.lose literals.lose in
          let dice_l = create_dice context Id.dice_l a ~idx:1 in
          let dice_r = create_dice context Id.dice_r b ~idx:2 in
          let result =
            if hand = Cho && (a + b) mod 2 = 0
              || hand = Han && (a + b) mod 2 <> 0
            then label_win
            else label_lose
          in
          spawn_r [dice_l; dice_r; result]
end

let _ =
  Game.add_scene chohan "agreement" (module Agreement);
  Game.add_scene chohan "main" (module GameMain);
  start chohan "agreement" 
