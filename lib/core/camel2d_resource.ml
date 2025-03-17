module Image = Camel2d_resource_image
module Audio = Camel2d_resource_audio
module Font = Camel2d_resource_font

type label = int

let gen_label =
  let cnt = ref Int.min_int in
  fun () -> cnt := !cnt + 1; !cnt

type bucket = {
  images: (label, Image.t) Hashtbl.t;
  audios: (label, Audio.t) Hashtbl.t;
  fonts: (label, Font.t) Hashtbl.t;
}

type audio_mode = SE | BGM
type request = {
  image_root: string;
  image_requests: (label * string) list;
  audio_root: string;
  audio_mode: audio_mode;
  audio_requests: (label * string * bool) list;
  font_root: string;
  font_requests: (label * string) list;
}
type 'a factory = request -> 'a * request

let return a = fun request -> (a, request)
let bind t ~f = fun request ->
  let a, request = t request in
  f a request
let (let*) t f = bind t ~f
let (>>) t1 t2 = let* _ = t1 in t2

let get = fun request -> (request, request)
let put request = fun _ -> ((), request)

let set_image_root path =
  let* request = get in
  put {request with image_root = path}

let set_audio_root path =
  let* request = get in
  put {request with audio_root = path}

let set_font_root path =
  let* request = get in
  put {request with font_root = path}

let set_audio_mode mode =
  let* request = get in
  put {request with audio_mode = mode}

let load_image name path =
  let* request = get in
  let path = request.image_root ^ path in
  let image_requests = (name, path) :: request.image_requests in
  put {request with image_requests}

let load_audio name path =
  let* request = get in
  let path = request.audio_root ^ path in
  let loop = if request.audio_mode = BGM then true else false in
  let audio_requests = (name, path, loop) :: request.audio_requests in
  put {request with audio_requests}

let load_font name path =
  let* request = get in
  let path = request.font_root ^ path in
  let font_requests = (name, path) :: request.font_requests in
  put {request with font_requests}

let run context t =
  let init = {
    image_root="";
    image_requests=[];
    audio_root="";
    audio_mode=BGM;
    audio_requests=[];
    font_root="";
    font_requests=[]
  } in
  let (), {image_requests; audio_requests; font_requests; _} = t init in
  let open Promise in
  let load_img (name, path) =
    let* img = Image.load path in return (name, img) in
  let load_audio (name, path, is_loop) =
    let* audio = Audio.load ~context ~is_loop path in return (name, audio) in
  let load_font (name, path) =
    let* font = Font.load path in return (name, font) in
  let* images = List.sequence @@ List.map load_img image_requests in
  let* audios = List.sequence @@ List.map load_audio audio_requests in
  let* fonts  = List.sequence @@ List.map load_font font_requests in
  let image_bucket = Hashtbl.create 10 in
  let audio_bucket = Hashtbl.create 10 in
  let font_bucket = Hashtbl.create 10 in
  List.iter (fun (name, img) -> Hashtbl.add image_bucket name img) images;
  List.iter (fun (name, audio) -> Hashtbl.add audio_bucket name audio) audios;
  List.iter (fun (name, font) -> Hashtbl.add font_bucket name font) fonts;
  return { images = image_bucket; audios = audio_bucket; fonts = font_bucket }

let fetch_image bucket name = Hashtbl.find bucket.images name
let fetch_audio bucket name = Hashtbl.find bucket.audios name
let fetch_font bucket name = Hashtbl.find bucket.fonts name
