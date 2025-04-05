module TextStyle = Camel2d_preset_textstyle
module Basic = Camel2d_preset_basic
module AudioAgreement = Camel2d_preset_audio_agreement
module Animation = Camel2d_preset_animation

module Scene = struct
  module SceneAudioAgreement = struct
    module Make(Params: sig
      val next_scene: string
    end): Camel2d_scene.T = struct
      type t = AudioAgreement.t
      let load_resources = Camel2d_resource.return ()
      let init _ = AudioAgreement.init
      let renderer = AudioAgreement.render
      let sound_mixer = Camel2d_sound_mixer.return
      let updater t =
        let open Camel2d_updater in
        let open AudioAgreement in
        let* t = update t in
        if clicked t
        then start_scene Params.next_scene
        else return t    
      let event_handler = AudioAgreement.handle_event
    end
  end
end