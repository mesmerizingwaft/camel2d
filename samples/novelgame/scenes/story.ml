open Camel2d

type action =
  | CPass
  | CStartScene of string

type line =
  | UpdateImage of Resource.label * string * string
  | PlaySE of Resource.label * string
  | UpdateBGM of Resource.label * string
  | UpdateText of string
  | StartScene of string
  | Choice of (string * action) list

let load_choice lines =
  let rec inner choices lines =
    match lines with
      | [] -> Choice choices, []
      | line::lines ->
        if Unicode.sub line 0 1 <> "-"
        then Choice choices, (line::lines)
        else
          let line = Unicode.sub line 1 (Unicode.length line - 1) in
          let line = String.trim line in
          let tokens = String.split_on_char ' ' line in
          let tokens = List.map String.trim tokens in
          match tokens with
            | [label; "pass"] ->
              inner ((label, CPass)::choices) lines
            | [label; "start_scene"; scene_name] ->
              inner ((label, CStartScene scene_name)::choices) lines
            | _ -> failwith (Printf.sprintf "Parse Error:[%s]" (String.concat ";" tokens))
  in
  inner [] lines

let load_script script =
  let rec parse acc lines =
    match lines with
      | [] -> List.rev acc
      | line::lines -> begin
        let tokens = String.split_on_char ' ' line in
        match tokens with
          | ["#img"; id; filename] ->
            parse (UpdateImage (Resource.gen_label (), id, filename)::acc) lines
          | ["#se"; filename] ->
            parse (PlaySE (Resource.gen_label (), filename)::acc) lines
          | ["#bgm"; filename] ->
            parse (UpdateBGM (Resource.gen_label (), filename)::acc) lines
          | ["#start_scene"; scene_name] ->
            parse (StartScene scene_name::acc) lines
          | ["#choice"] ->
            let choice, lines = load_choice lines in
            parse (choice::acc) lines
          | _ ->
            let text = String.concat " " tokens in
            parse (UpdateText text::acc) lines
      end
  in
  parse [] (String.split_on_char '\n' script)

let load_resources_from_script lines =
  let open Resource in
  let rec aux = function
    | [] -> return ()
    | UpdateImage (label, _, path) :: rest ->
      load_image label path
      >> aux rest
    | PlaySE (label, path) :: rest ->
      set_audio_mode SE
      >> load_audio label path
      >> aux rest
    | UpdateBGM (label, path) :: rest ->
      set_audio_mode BGM
      >> load_audio label path
      >> aux rest
    | UpdateText _ :: rest -> aux rest
    | StartScene _scene_name :: rest -> aux rest
    | Choice _ :: rest -> aux rest
  in
  set_image_root "/samples/novelgame/static/imgs/"
  >> set_audio_root "/samples/novelgame/static/audio/"
  >> aux lines

module Make(Script: sig
  val script: line list
end): Scene.T = struct
  type t = {
    script: line list;
    message_box: Preset.Adv.MessageBox.t;
    bg: Preset.Basic.Image.t option;
    choices: (Choice.t * action) list;
  }

  module ResourceLabels = struct
    open Resource
    let font_tamanegi = gen_label ()
    let bg_message_box = gen_label ()
  end

  let load_resources =
    let open Resource in
    load_resources_from_script Script.script
    >> set_font_root "/samples/novelgame/static/fonts/"
    >> load_font ResourceLabels.font_tamanegi "tamanegi_v7.ttf"
    >> set_image_root  "/samples/novelgame/static/imgs/"
    >> load_image ResourceLabels.bg_message_box "hakkou2.png"
    >> Choice.load_resources

  let _ = Script.script
  let init context =
    let sw, sh = Context.get_canvas_size context in
    let text_style =
      Preset.TextStyle.create ()
      |> Preset.TextStyle.set_fontface (ResourceLabels.font_tamanegi)
      |> Preset.TextStyle.set_color (RGB (255, 255, 255))
      |> Preset.TextStyle.set_outline (Edging (RGB (0, 0, 0)))
      |> Preset.TextStyle.set_font_size 30
    in
    let message_box =
      let open Preset.Adv.MessageBox in
      create ~x:10 ~y:(sh - 210) ~w:(sw-20) ~h:200 ~text_style ~label_bg:ResourceLabels.bg_message_box ()
      |> set_padding ~left:30 ~top:30 ~right:30 ~bottom:30
      |> enable_typewriter_effect ~character_per_sec:500
    in
    {
      script = Script.script;
      message_box;
      bg = None;
      choices = [];
    }
  let sound_mixer t = SoundMixer.return t

  let render_choices t =
    let open Renderer in
    List.fold_left (fun acc (choice, _) ->
      let open Renderer in
      acc >> Choice.render choice
    ) (return ()) t.choices

  let renderer t =
    let open Renderer in
    Option.map (fun bg -> Preset.Basic.Image.render bg) t.bg
    |> Option.value ~default:(return ())
    >> Preset.Adv.MessageBox.render t.message_box
    >> render_choices t

  let rec read_new_line t =
    let open Updater in
    let* sw, sh = get_canvas_size in
    match t.script with
      | [] -> return t
      | (UpdateText text)::lines ->
        let message_box = Preset.Adv.MessageBox.send_text text t.message_box in
        return {t with message_box; script = lines}
      | (UpdateBGM (label, _))::lines ->
        SoundMixer.play_sound label >>
        read_new_line {t with script = lines}
      | (UpdateImage (label, _, _))::lines ->
        let bg = Some (Preset.Basic.Image.create_wh ~x:0 ~y:0 ~w:sw ~h:sh label) in
        read_new_line {t with bg; script = lines}
      | (StartScene name)::_ -> start_scene name
      | (Choice choices)::lines ->
        begin
          List.fold_left (fun acc (choice, _) ->
            acc >> print_endline choice
          ) (return ()) choices
        end
        >>
        let style = Preset.TextStyle.create ()
          |> Preset.TextStyle.set_fontface (ResourceLabels.font_tamanegi)
          |> Preset.TextStyle.set_color (RGB (255, 255, 255))
          |> Preset.TextStyle.set_outline (Edging (RGB (0, 0, 0)))
          |> Preset.TextStyle.set_font_size 30
        in
        let center_y = sh / 2 in
        let gap = 5 in
        let n_choices = List.length choices in
        let total_height = 
          n_choices * 64 + (n_choices - 1) * gap in
        let start_y = center_y - total_height / 2 in
        let choices = List.mapi (fun i (label, action) ->
          let x = sw / 2 in
          let y = start_y + i * (64 + gap) in
          (Choice.create ~style ~x ~y label ()), action
        ) choices in
        return {t with choices; script = lines}
      | _::lines -> read_new_line {t with script = lines}

  let update_components e t =
    let open Updater in
    let* message_box = Preset.Adv.MessageBox.update e t.message_box in
    let* choices = List.fold_left (fun choices (choice, action) ->
      let* choices = choices in
      let* choice = Choice.update e choice in
      return ((choice, action)::choices)
    ) (return []) t.choices
    in
    return {t with message_box; choices}

  let updater e t =
    let open Updater in
    let* t = update_components e t in
    match e with
      | Event.Tick ->
        let selected_choice = List.find_opt (fun (choice, _) -> Choice.clicked choice) t.choices in
        Option.map (fun (_, action) ->
          match action with
            | CPass ->
              let choices = [] in
              read_new_line {t with choices}
            | CStartScene scene_name ->
              start_scene scene_name
        ) selected_choice
        |> Option.value ~default:(return t)
      | Event.MouseUp _ when t.choices = [] ->
        read_new_line t
      | _ -> return t

end