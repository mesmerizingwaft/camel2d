open Camel2d

module ResourceLabels = struct
  open Resource
  let logo = gen_label ()
end

type t = Preset.Animation.FadingImage.t

let load_resources =
  let open Resource in
  let open ResourceLabels in
  set_image_root "/samples/novelgame/static/imgs/"
  >> load_image logo "logo.png"

let init context =
  let sw, sh = Context.get_canvas_size context in
  let w, h = 239, 104 in
  let x, y= (sw - w)/ 2, (sh - h) / 2 in
  let f =
    Preset.Animation.FadingImage.linear_fadeinout
    ~fadein_by:30 ~fadeout_from:90 ~frame_end:120
  in
  let logo = Preset.Animation.FadingImage.create
    ~x ~y ~f ResourceLabels.logo in
  logo

let sound_mixer t =
  let open SoundMixer in
  return t

let renderer t =
  Preset.Animation.FadingImage.render t

let updater e t =
  let open Updater in
  let* t = Preset.Animation.FadingImage.update e t in
  match e with
    | Event.Tick when Preset.Animation.FadingImage.current_frame t > 120 ->
      start_scene "title"
    | _ -> return t

