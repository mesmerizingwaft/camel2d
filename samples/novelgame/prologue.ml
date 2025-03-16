open Camel2d

module Script = struct
  type line =
    | Bg of Resource.label * string
    | Text of string

  let script =
    [%blob "prologue.txt"]
    |> String.split_on_char '\n'
    |> List.map (fun line ->
      if String.sub line 0 3 = "#bg" then
        let label = Resource.gen_label () in
        let file_name = String.sub line 4 (String.length line - 4) |> String.trim in
        Bg (label, file_name)
      else
        Text line)

  let load_resources =
    let open Resource in
    let rec aux = function
      | [] -> return ()
      | Bg (label, path) :: rest ->
        load_image label path
        >> aux rest
      | Text _ :: rest -> aux rest
    in
    set_image_root "/samples/novelgame/static/imgs/"
    >> aux script

end

let script = Script.script |> Array.of_list

let make (game : Game.t) : (module Scene.T) = (module struct
  let sw, sh = game.width, game.height
  let line_no = ref 0

  module Id = struct
    let bg = "bg"
  end

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
    let* line_no' = use_ref line_no in
    match script.(line_no') with
    | Text text ->
      put_text text
      >> put_ref line_no (line_no' + 1)
    | Bg (label, _) ->
      remove_when Condition.(has_id Id.bg)
      >> spawn [Entity.SingleImage.create Id.bg label ~pos:(0, 0) ~size:(sw, sh) ~z_index:(-1)]
      >> put_ref line_no (line_no' + 1)

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