
let radius = 50

module AudioAgreement = struct
  module Make(Params: sig
    val next_scene: string
  end): Camel2d_scene.T = struct
    type t = {
      btn_color: Camel2d_color.t;
    }
    let load_resources = Camel2d_resource.return ()
    let init _ = {
      btn_color = Camel2d_color.RGB (120, 120, 120)
    }
    let renderer t =
      let open Camel2d_renderer in
      let* w, h = get_canvas_size in
      let color = t.btn_color in
      Symbol.draw_play_button ~x:(w / 2) ~y:(h / 2) ~radius ~color

    let sound_mixer = Camel2d_sound_mixer.return

    let check_inclusion x y =
      let open Camel2d_updater in
      let open Camel2d_preset_collision_detection in
      let* w, h = get_canvas_size in
      return (Circle.check_inclusion ~x:(w/2) ~y:(h/2) ~radius x y)

    let updater e t =
      let open Camel2d_updater in
      match e with
        | Camel2d_event.MouseUp {x; y; _} ->
          let* is_clicked = check_inclusion x y in
          if is_clicked
          then begin
            resume_audio
            >> start_scene Params.next_scene
          end else return t
        | Camel2d_event.MouseMove {x; y} ->
          let* is_on = check_inclusion x y in
          if is_on
          then return {btn_color = Camel2d_color.RGB (255, 255, 255)}
          else return {btn_color = Camel2d_color.RGB (120, 120, 120)}
        | _ -> return t

  end
end