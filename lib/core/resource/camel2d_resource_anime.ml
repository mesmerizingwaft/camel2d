open Js_of_ocaml

type t = {
  msec_to_index: int -> int;
  frames: Dom_html.imageElement Js.t array
}

let load src =
  let promise, resolver = Promise.make () in
  let req = XmlHttpRequest.create () in
  Js.Unsafe.meth_call req "open" [|Js.Unsafe.coerce (Js.string "GET"); Js.Unsafe.coerce (Js.string src); Js.Unsafe.coerce Js._true|] |> ignore;
  req##.responseType := Js.string "arraybuffer";
  Js.Unsafe.set req "onloadend" (Dom_html.handler (fun _ ->
    if req##.status = 404
    then let open Camel2d_error_types in
    Promise.reject resolver (ResourceNotAvailable ("anime", src));
    Js._false
  else
    let array_buffer = Js.Unsafe.get req "response" in
    let byte_array = Js.Unsafe.new_obj Js.Unsafe.global##.Uint8Array [|Js.Unsafe.coerce array_buffer|] in
    let ocmal_bytes = Bytes.create (byte_array##.length) in
    for i = 0 to byte_array##.length - 1 do
      Bytes.set ocmal_bytes i (char_of_int (byte_array##at i))
    done;
    let gif = Gif.from_bytes ocmal_bytes in
    let frames = Gif.slice_animated_gif gif in
    let frames = List.map (fun frame ->
      let promise, resolver = Promise.make () in
      let img = Dom_html.createImg Dom_html.document in
      let encoded_frame =
        Gif.to_bytes frame
        |> Bytes.to_string
        |> Base64.encode_exn
      in
      img##.src := Js.string ("data:image/gif;base64," ^ encoded_frame);
      img##.onload := Dom_html.handler (fun _ ->
        Promise.resolve resolver (Gif.delay_time_of frame, img);
        Js._false
      );
      promise
    ) frames in
    let open Promise in
    continue_after_resolved (List.sequence frames) (fun frames ->
      let msec_to_index msec =
        let max_msec =
          List.fold_left (fun acc (delay_time, _) ->
            acc + delay_time
          ) 0 frames
        in
        let rec aux msec frames =
          match frames with
          | [] -> failwith "no frame"
          | (delay_time, _) :: xs ->
            if msec < delay_time then 0
            else 1 + aux (msec - delay_time) xs
        in
        aux (msec mod max_msec) frames
      in
      let frames = Array.of_list (List.map snd frames) in
      resolve resolver {frames; msec_to_index};
    );
    Js._false
  ));
  req##send Js.null;
  promise

let render ?(alpha=1.0) ~context ~x ~y ?(w=None) ?(h=None) (img: t) msec =
  let ctx = Camel2d_context.get_context2d context in
  let frame_index = img.msec_to_index msec in
  let x, y = float_of_int x, float_of_int y in
  let w = Option.(map (fun w -> float_of_int w) w |> value ~default:(float_of_int img.frames.(frame_index)##.width)) in
  let h = Option.(map (fun h -> float_of_int h) h |> value ~default:(float_of_int img.frames.(frame_index)##.height)) in
  ctx##save;
  ctx##.globalAlpha := alpha;
  ctx##drawImage_withSize img.frames.(frame_index) x y w h;
  ctx##restore

let width_of (img: t) msec =
  let frame_index = img.msec_to_index msec in
  img.frames.(frame_index)##.width
let height_of (img: t) msec =
  let frame_index = img.msec_to_index msec in
  img.frames.(frame_index)##.height
  (*
  req##.onload := Dom_html.handler (fun _ ->
    let buffer = req##.response in
    let array = Js.to_array (Js.Unsafe.global##.Uint8Array buffer) in
    let frames = Js.array (Array.map (fun x -> Js.Unsafe.global##.Image x) array) in
    let img = Js.Unsafe.global##.Image frames in
    img##.onload := Dom_html.handler (fun _ ->
      Promise.resolve resolver img;
      Js._false
    );
    Js._false
  );
  req##open_ (Js.string "GET") (Js.string src) Js._false;
  req##send Js.null;
   
*)