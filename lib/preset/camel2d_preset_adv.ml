module MessageBox = struct

  type typewriter_efffect = {
    character_per_sec: int
  }

  type t = {
    bg: Camel2d_preset_basic.Image.t;
    padding_left: int;
    padding_right: int;
    padding_top: int;
    padding_bottom: int;
    typewriter_efffect: typewriter_efffect option;
    text_style: Camel2d_preset_textstyle.t;
    remain_text: string;
    displayed_texts: string list;
  }

  let create ~x ~y ~w ~h ~text_style ~label_bg () =
    let bg = Camel2d_preset_basic.Image.create_wh ~x ~y ~w ~h label_bg in
    let padding_left = 0 in
    let padding_right = 0 in
    let padding_top = 0 in
    let padding_bottom = 0 in
    let typewriter_efffect = None in
    let remain_text = "" in
    let displayed_texts = [""] in
    { 
      bg;
      padding_left;
      padding_right;
      padding_top;
      padding_bottom;
      typewriter_efffect;
      remain_text;
      displayed_texts;
      text_style
    }

  let set_padding ~left ~right ~top ~bottom t =
    {t with padding_left=left; padding_right=right; padding_top=top; padding_bottom=bottom}

  let enable_typewriter_effect ~character_per_sec t =
    let typewriter_efffect = Some {character_per_sec} in
    {t with typewriter_efffect}

    let render_text t =
      let open Camel2d_renderer in
      let rec inner i = function
        | [] -> return ()
        | text :: texts ->
          let box_x, box_y =
            let open Camel2d_preset_basic.Image in
            x_of t.bg, y_of t.bg
          in
          let x = box_x + t.padding_left in
          let y = box_y + t.padding_top + (i * t.text_style.pt) in
          Text.draw ~x ~y text
          >> inner (i+1) texts
      in
      clear_text_style
      >> set_font_size t.text_style.pt
      >> set_color t.text_style.color
      >> set_outline t.text_style.outline
      >> set_letter_spacing t.text_style.letter_spacing
      >> Option.value (
        Option.map (fun label -> set_font_face label) t.text_style.font_face
      ) ~default:(return ())
      >> inner 0 (List.rev t.displayed_texts)
    
    let render t =
      let open Camel2d_renderer in
      Camel2d_preset_basic.Image.render t.bg
      >> render_text t

  let newline_required t text =
    let open Camel2d_updater in
    let max_w = Camel2d_preset_basic.Image.w_of t.bg - t.padding_left - t.padding_right in
    clear_text_style
    >> set_font_size t.text_style.pt
    >> set_letter_spacing t.text_style.letter_spacing
    >> Option.value (
      Option.map (fun label -> set_font_face label) t.text_style.font_face
    ) ~default:(return ())
    >> let* w, _ = get_text_size text in
    return (w > max_w)

  let rec load_new_char_with_typewriter_effect n t =
    let open Camel2d_updater in
    if n <= 0 || t.remain_text = "" then return t
    else
      let new_char = Unicode.sub t.remain_text 0 1 in
      let new_text = List.hd t.displayed_texts ^ new_char in
      let* is_newline = newline_required t new_text in
      let displayed_texts = if is_newline then
        new_char :: t.displayed_texts
      else
        new_text :: List.tl t.displayed_texts
      in
      let remain_text =
        Unicode.sub t.remain_text 1 (Unicode.length t.remain_text - 1)
      in
      let t = {t with displayed_texts; remain_text} in
      load_new_char_with_typewriter_effect (n-1) t

  let load_new_char t =
    match t.typewriter_efffect with
      | None -> failwith "unimplemented"
      | Some {character_per_sec} ->
        let character_per_frame = Int.min (Int.max (character_per_sec / 60) 1) (Unicode.length t.remain_text) in
        load_new_char_with_typewriter_effect character_per_frame t

  let update e t =
    let open Camel2d_updater in
    match e with
      | Camel2d_event.Tick ->
        let* bg = Camel2d_preset_basic.Image.update e t.bg in
        let* t = load_new_char t in
        return {t with bg}
      | _ -> return t

  let send_text text t =
    let remain_text = text in
    {t with remain_text; displayed_texts=[""]}
end