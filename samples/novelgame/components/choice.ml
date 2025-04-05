open Camel2d

type t = {
  bg: Preset.Basic.Image.t;
  label_text: Preset.Basic.Text.t;
}

module ResourceLabels = struct
  open Resource
  let bg = gen_label ()
  let bg_selected = gen_label ()
end

let load_resources =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/novelgame/static/imgs/"
  >> load_image bg "hakkou_mini.png"
  >> load_image bg_selected "hakkou_mini2.png"

let create ~x ~y ~style label_text () =
  let open Preset.Basic in
  let w, h = 239, 64 in
  let bg =
    let x, y = x - w / 2, y - h / 2 in
    Image.create_wh ~x ~y ~w ~h ResourceLabels.bg
  in
  let label_text = Text.create_centerized ~style ~x ~y label_text in
  {bg; label_text}

let render t =
  let open Renderer in
  Preset.Basic.Image.render t.bg
  >> Preset.Basic.Text.render t.label_text

let update t =
  let open Updater in
  let* bg = Preset.Basic.Image.update t.bg in
  return {t with bg}

let handle_event ev t =
  let open Updater in
  let* bg = Preset.Basic.Image.handle_event ev t.bg in
  let t = {t with bg} in
  match ev with
  | Event.MouseMove {x; y} ->
    let* mouse_on = Preset.Basic.Image.check_inclusion t.bg x y in
    if mouse_on then
      let bg = Preset.Basic.Image.update_label ResourceLabels.bg_selected t.bg in
      return {t with bg}
    else
      let bg = Preset.Basic.Image.update_label ResourceLabels.bg t.bg in
      return {t with bg}
  | _ -> return t

let clicked t = Preset.Basic.Image.clicked t.bg
let unclick t = {t with bg = Preset.Basic.Image.unclick t.bg}