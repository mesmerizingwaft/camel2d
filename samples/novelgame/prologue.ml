open Camel2d

module Script = struct
  type line =
    | UpdateImage of Resource.label * string * string
    | PlaySE of Resource.label * string
    | UpdateBGM of Resource.label * string
    | UpdateText of string

  let script =
    [%blob "prologue.txt"]
    |> String.split_on_char '\n'
    |> List.map (fun line ->
      match String.split_on_char ' ' line with
        | ["#img"; id; filename] -> UpdateImage (Resource.gen_label (), id, filename)
        | ["#se"; filename] -> PlaySE (Resource.gen_label (), filename)
        | ["#bgm"; filename] -> UpdateBGM (Resource.gen_label (), filename)
        | _ -> UpdateText line)

  let load_resources =
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
    in
    set_image_root "/samples/novelgame/static/imgs/"
    >> set_audio_root "/samples/novelgame/static/audio/"
    >> aux script

end

let script = Script.script |> Array.of_list

let make (game : Game.t) : (module Scene.T) = (module struct
  let sw, sh = game.width, game.height
  let line_no = ref 0

  module ResourceLabels = struct
    open Resource
    let bg_message_box = gen_label ()
  end

  let load_resources =
    let open Resource in
    let open ResourceLabels in
    set_image_root "/samples/novelgame/static/imgs/"
    >> load_image bg_message_box "hakkou2.png"
    >> Script.load_resources

  let bg_message_box =
    let open Entity in
    let w, h = sw - 20, 200 in
    let pos = 10, sh - 210 in
    SingleImage.create "bg_message_box" ResourceLabels.bg_message_box ~pos ~size:(w, h)

  let {
    Assets.MessageWindow.put_text;
    type_all;
    remain_length;
    m;
    _
  } = Assets.MessageWindow.(
    let style = Entity.TextLabel.(create_style ~color:(RGBA (255, 255, 255, 1.)) 20) in
    create bg_message_box ~style ~padding_l:30 ~padding_t:30 ~padding_r:30 ~padding_b:30
  )

  module MessageWindow = (val m)

  let load_next_page =
    let open World in
    let rec inner () =
      let* line_no' = use_ref line_no in
      match script.(line_no') with
      | UpdateText text ->
        put_text text
        >> put_ref line_no (line_no' + 1)
      | PlaySE (label, _) ->
        play_audio label
        >> put_ref line_no (line_no' + 1)
        >> (let* _ = return () in inner())
      | UpdateBGM (label, _) ->
        play_audio label
        >> put_ref line_no (line_no' + 1)
        >> (let* _ = return () in inner())
      | UpdateImage (label, id, _) ->
        remove_when Condition.(has_id id)
        >> spawn [Entity.SingleImage.create id label ~pos:(0, 0) ~size:(sw, sh) ~z_index:(-1)]
        >> put_ref line_no (line_no' + 1)
        >> (let* _ = return () in inner())
    in inner ()

  let initialize context =
    let open World in
    put_ref line_no 0
    >> MessageWindow.initialize context
    >> load_next_page

  let handle_event _context ev =
    let open World in
    match ev with
      | Event.KeyUp { key_code = 32 } ->
        ifm (let* l = remain_length in return (l <= 0))
          (load_next_page)
          (type_all)
      | _ -> return ()

  let update context =
    MessageWindow.update context
end)