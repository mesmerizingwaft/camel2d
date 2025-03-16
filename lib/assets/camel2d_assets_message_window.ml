
type t = {
  put_text: string -> unit Camel2d_world.t;
  clear_text: unit -> unit Camel2d_world.t;
  type_all: unit Camel2d_world.t;
  remain_length: int Camel2d_world.t;
  m: (module Camel2d_assets_type.T);
}

let create
  ?(typewriter_effect=true)
  ?(typewriter_speed=5)
  (bg_entity: Camel2d_entity.t)
  ~(style: Camel2d_entity.TextLabel.text_style)
  ~padding_l ~padding_t ~padding_r ~padding_b =
  let _ = padding_b in
  let _frame_cnt = ref 0 in
  let x, y = Camel2d_entity_types.(bg_entity.x, bg_entity.y) in
  let x, y = x + padding_l, y + padding_t in
  let _text_buffer = ref "" in
  let _line_of_texts = ref [""] in
  let put_text text =
    let open Camel2d_world in
    put_ref _text_buffer text
    >> put_ref _line_of_texts [""]
  in
  let clear_text () =
    let open Camel2d_world in
    put_ref _text_buffer ""
    >> put_ref _line_of_texts [""]
  in
  let is_newline_required text =
    let e = Camel2d_entity.TextLabel.create ~style ~pos:(x,y) text text in
    let w = Camel2d_entity_types.(e.w) in
    let max_w = Camel2d_entity_types.(bg_entity.w - padding_l - padding_r) in
    w > max_w
  in
  let load_new_char =
    let open Camel2d_world in
    let* text_buffer = use_ref _text_buffer in
    let* line_of_texts = use_ref _line_of_texts in
    match text_buffer with
    | "" -> return ()
    | text ->
      let text = if typewriter_effect then
        Unicode.sub text 0 1
      else
        text
      in
      let new_text = List.hd line_of_texts ^ text in
      let is_newline = is_newline_required new_text in
      let new_line_of_texts = if is_newline then
        text :: line_of_texts
      else
        new_text :: List.tl line_of_texts
      in
      put_ref _text_buffer (Unicode.sub text_buffer 1 (Unicode.length text_buffer - 1))
      >> put_ref _line_of_texts new_line_of_texts
  in
  let spawn_line_of_texts =
    let open Camel2d_world in
    let* line_of_texts = use_ref _line_of_texts in
    let entities = List.mapi (fun i text ->
      let y = y + i * style.pt in
      Camel2d_entity.TextLabel.create ~style ~pos:(x,y) "__line_of_texts" text
    ) (List.rev line_of_texts) in
    spawn entities
  in
  let remain_length =
    let open Camel2d_world in
    let* text_buffer = use_ref _text_buffer in
    return (Unicode.length text_buffer)
  in
  let type_all =
    let open Camel2d_world in
    let rec loop () =
      ifm (let* l = remain_length in return (l <= 0))
        (return ())
        (let* _ = return () in load_new_char >> loop ())
    in
    loop ()
  in
  let m : (module Camel2d_assets_type.T) = (module 
    struct
      let load_resources () = Camel2d_resource.return ()
      let initialize _context =
        let open Camel2d_world in
        spawn [bg_entity]
        >> clear_text ()
      let update _context =
        let open Camel2d_world in
        remove_when Condition.(has_id "__line_of_texts")
        >> (let* frame_cnt = use_ref _frame_cnt in
            if frame_cnt < typewriter_speed then
              put_ref _frame_cnt (frame_cnt + 1)
            else
              put_ref _frame_cnt 0
              >> load_new_char)
        >> spawn_line_of_texts
      let handle_event _context _ev = Camel2d_world.return ()
    end
  ) in
  {
    put_text;
    clear_text;
    type_all;
    remain_length;
    m;
  }