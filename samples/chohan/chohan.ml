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
  let next_scene = "main"
end)

module GameMain : Scene.T = struct
  module ResourceLabels = struct
    open Resource
    let bgm1 = gen_label ()
    let bgm2 = gen_label ()
    let se_win = gen_label ()
    let se_lose = gen_label ()
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
    >> set_audio_mode SE
    >> load_audio se_lose "lose.mp3"
    >> load_audio se_win "win.mp3"
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
    let open Entity.TextLabel in
    let pt = 100 in
    let style = create_style pt in
    let pos = (chohan.width / 3 * idx, (chohan.height - pt) / 2) in
    create ~context ~style ~pos ~base_horizontal: BHCenter id literals.dice_face.(number - 1)

  let create_popup_text label text =
    let pt = 200 in
    let style =
      let open Entity.TextLabel in
      let font_face = Some "tamanegi" in
      let outline = Edging (RGBA (0, 0, 0, 1.)) in
      create_style ~font_face ~outline pt
    in
    let pos = (chohan.width / 2, (chohan.height - pt) / 2) in
    let f t = (5 - t) * 10 in
    Assets.PopupText.create
      ~style
      ~pos
      ~f
      ~base_horizontal:PopupText.BHCenter
      label text

  module LabelWin = (val create_popup_text Id.win literals.win)
  module LabelLose = (val create_popup_text Id.lose literals.lose)

  (* Logic *)
  type hand = Cho | Han
  type phase =
    | Init
    | DiceRolling of hand * int
    | Win of int * int
    | Lose of int * int

  let str_of_phase = function
    | Init -> "Init"
    | DiceRolling _ -> "DiceRolling"
    | Win _ -> "Win"
    | Lose _ -> "Lose"
  
  let phase = ref Init
  let roll () = (Random.int 6) + 1
  let roll_twice () = (roll (), roll ())
  let update_phase new_phase =
    let open World in
    print_endline ("new_phase=" ^ str_of_phase new_phase)
    >> put_ref phase new_phase    
  let check_win hand a b =
    hand = Cho && (a + b) mod 2 = 0 || hand = Han && (a + b) mod 2 <> 0
  let use_phase = World.use_ref phase

  let initialize context =
    let sw, sh = chohan.width, chohan.height in
    let bg, fg =
      let open Entity in
      let l_bg = ResourceLabels.bg in
      let l_fg = ResourceLabels.fg in
      let bg = SingleImage.create Id.bg l_bg ~pos:(0, 0) ~size:(sw, sh) in
      let fg = let (w, h) = (int_of_float (0.75 *. float_of_int sh), sh) in
        SingleImage.create Id.fg l_fg ~pos:((sw - w) / 2, 0) ~size:(w, h) in
      bg, fg
    in
    let button_cho, button_cho_on_mousehover =
    let open Entity in
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
    let open Entity in
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
      let open Entity in
      let open TextLabel in
      let style =
        let font_face = Some "tamanegi" in
        let outline = Edging (RGBA (0, 0, 0, 1.)) in
        create_style ~font_face ~outline 25
      in
      create ~context ~style ~pos:((sw - 25 * 21) / 2, 20) Id.speech literals.speech
    in
    let open World in
    update_phase Init
    >> play_audio ResourceLabels.bgm1
    >> spawn [
      bg;
      fg;
      speech;
      button_cho; (Entity.hide button_cho_on_mousehover);
      button_han; (Entity.hide button_han_on_mousehover)
    ]
    >> LabelWin.initialize context
    >> LabelLose.initialize context

  let handle_event _context ev =
    let open World in
    let init_handler = function
      | Event.MouseMove {x; y} ->
        let is_button_default = Condition.((has_id Id.button_cho) ||| (has_id Id.button_han)) in
        let is_button_4_hover = Condition.((has_id Id.button_cho_mousehover) ||| (has_id Id.button_han_mousehover)) in
        let is_mouse_on = Condition.is_in x y in
        update_when is_button_default Updator.show
        >> update_when is_button_4_hover Updator.hide
        >> update_when Condition.(is_button_default &&& is_mouse_on) Updator.hide
        >> update_when Condition.(is_button_4_hover &&& is_mouse_on) Updator.show
      | Event.MouseUp {x; y; _} ->
        let* cho_clicked = exists Condition.(has_id Id.button_cho &&& is_in x y) in
        let* han_clicked = exists Condition.(has_id Id.button_han &&& is_in x y) in
        let button_clicked = cho_clicked || han_clicked in
        let is_button = Condition.(any_of (List.map has_id Id.[button_cho; button_cho_mousehover; button_han; button_han_mousehover])) in
        let next_phase =
          if cho_clicked then DiceRolling(Cho, 0)
          else if han_clicked then DiceRolling(Han, 0)
          else Init
        in
        update_phase next_phase
        >> (if button_clicked then stop_audio ResourceLabels.bgm1 else return ())
        >> (if button_clicked then play_audio ResourceLabels.bgm2 else return ())
        >> update_when Condition.(lift button_clicked &&& is_button) Updator.hide
      | _ -> return ()
    in
    let final_handler = function
      | Event.MouseUp _ -> start_scene "main"
      | _ -> return ()
    in
    let* phase = use_phase in
    match phase with
      | Init -> init_handler ev
      | DiceRolling _ -> return ()
      | Win _ | Lose _ -> final_handler ev

  let update context =
    let open World in
    let* phase = use_phase in
    match phase with
      | Init -> return ()
      | DiceRolling (hand, counter) ->
        let a, b = roll_twice () in
        let dice_l = create_dice context Id.dice_l a ~idx:1 in
        let dice_r = create_dice context Id.dice_r b ~idx:2 in
        let* dice_exists = exists Condition.(has_id Id.dice_l ||| has_id Id.dice_r) in
        let next_phase = if counter <= 60
          then DiceRolling (hand, counter + 1)
          else if check_win hand a b then Win (a, b)
          else Lose (a, b)
        in
        let replace_dice =
          replace_by_id Id.dice_l dice_l
          >> replace_by_id Id.dice_r dice_r
        in
        update_phase next_phase
        >> (if dice_exists then replace_dice else spawn [dice_l; dice_r])
        >> (match next_phase with
          | Win _ ->
            stop_audio ResourceLabels.bgm2
            >> play_audio ResourceLabels.se_win
          | Lose _ ->
            stop_audio ResourceLabels.bgm2
            >> play_audio ResourceLabels.se_lose
          | _ -> return ())
      | Win (_a, _b) -> LabelWin.update context
      | Lose (_a, _b) -> LabelLose.update context
end

let _ =
  Game.add_scene chohan "agreement" (module Agreement);
  Game.add_scene chohan "main" (module GameMain);
  start chohan "agreement" 
