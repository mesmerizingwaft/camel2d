open Js_of_ocaml

type context = Camel2d_context.t
type resource_bucket = Camel2d_resource.bucket

type t = {
  id: string;
  render: t -> context -> resource_bucket -> unit;
  is_visible: bool;
  x: int;
  y: int;
  w: int;
  h: int;
}

let render context resource_bucket ({render; _} as t) =
  render t context resource_bucket

module SingleImage = struct
  let create ~pos ~size ?(is_visible=true) id resource_name =
    let (x, y), (w, h) = pos, size in
    let render {x; y; w; h; is_visible; _} context bucket =
      if is_visible then begin
        let img = Camel2d_resource.fetch_image bucket resource_name in
        Camel2d_resource.Image.render img context ~x ~y ~w ~h
      end
    in
    { id; render; is_visible; x; y; w; h }
end

module TextLabel: sig
  type color = RGBA of int * int * int * float
  type outline = NoOutline | Edging of color
  type base_horizontal = BHCenter | BHLeft
  type text_style = {
    pt: int;
    color: color;
    outline: outline;
    font_face: string option;
    letter_spacing: string;
  }

  val create_style :
    ?color: color ->
    ?outline: outline ->
    ?font_face: string option ->
    ?letter_spacing: string ->
    int -> text_style
  
  val text_width_of: context:Camel2d_context.t -> style:text_style -> string -> int
  val create: context: Camel2d_context.t -> style: text_style -> pos:(int * int) -> ?is_visible: bool -> ?base_horizontal: base_horizontal -> string -> string -> t
end = struct
  type color = RGBA of int * int * int * float
  type outline = NoOutline | Edging of color
  type base_horizontal = BHCenter | BHLeft
  type text_style = {
    pt: int;
    color: color;
    outline: outline;
    font_face: string option;
    letter_spacing: string;
  }

  let create_style
    ?(color=RGBA(255, 255, 255, 1.))
    ?(outline=NoOutline)
    ?(font_face=None)
    ?(letter_spacing="normal")
    pt =
    { pt; color; outline; font_face; letter_spacing }

  let _js_str_of_color = function
    | RGBA (r, g, b, a) -> Js.string @@ Printf.sprintf "rgba(%d,%d,%d,%f)" r g b a
    
  let _pt_as_px style = Printf.sprintf "%dpx" style.pt

  let _js_font_of style =
    let pt = _pt_as_px style in
    let font_face = Option.value style.font_face ~default:"caption" in
    Js.string @@ Printf.sprintf "%s %s" pt font_face

  let _with_style style context k =
    let ctx = Camel2d_context.get_context2d context in
    ctx##save;
    ctx##.fillStyle := _js_str_of_color style.color;
    ctx##.font := _js_font_of style;
    ctx##.textBaseline := Js.string "top";
    Ext.set_letter_spacing_of ctx style.letter_spacing;
    let result = k ctx in
    ctx##restore;
    result

  let _render_outline ctx outline text x y =
    (match outline with
    | NoOutline -> ()
    | Edging color ->
      ctx##save;
      ctx##.lineWidth := 3.;
      ctx##.fillStyle := _js_str_of_color color;
      ctx##strokeText (Js.string text) (float_of_int x) (float_of_int y);
      ctx##restore;
    )

  let text_width_of ~context ~style text =
    let text = Js.string text in
    _with_style style context (fun ctx -> (ctx##measureText text)##.width)
    |> int_of_float

  let create ~context ~style ~pos ?(is_visible=true) ?(base_horizontal=BHLeft) id text =
    let (x, y) = pos in
    (* ToDo: FONT needs to be async loaded *)
    let (w, h) = text_width_of ~context ~style text, style.pt in
    let render {x; y; is_visible; _} context _ =
      let (w, _) = text_width_of ~context ~style text, style.pt in
      if is_visible then begin
        _with_style style context (fun ctx ->
          let x = if base_horizontal = BHCenter
            then x - w / 2
            else x
          in
          _render_outline ctx style.outline text x y;
          ctx##fillText (Js.string text) (float_of_int x) (float_of_int y);
          ctx##restore
        )
      end
    in
    { id; render; is_visible; x; y; w; h }
end

let create_from_text
  ?(is_visible=true)
  ?(color=`RGBA(255, 255, 255, 1.))
  ?(outline=`NoOutline)
  ?(font_face=None)
  ?(letter_spacing="normal")
  ?(base_horizontal=`Left)
  ~pt ~pos ~size id text =
  let (x, y), (w, h) = pos, size in
  let render {x; y; is_visible; _} context _ =
    if is_visible then begin
      let ctx = Camel2d_context.get_context2d context in
      ctx##save;
      ctx##.fillStyle := begin
        match color with
          | `RGBA (r,g,b,a) ->
            Js.string @@ Printf.sprintf "rgba(%d,%d,%d,%f)" r g b a
          | _ -> failwith "invalid color"
      end;
      let pt = Printf.sprintf "%dpx" pt in
      let font_face = Option.value font_face ~default:"caption" in
      ctx##.font := Js.string @@ Printf.sprintf "%s %s" pt font_face;
      let x = if base_horizontal = `Center
        then x - (int_of_float ((ctx##measureText (Js.string text))##.width /. 2.))
        else x
      in
      ctx##.textBaseline := Js.string "top";
      Ext.set_letter_spacing_of ctx letter_spacing;
      (match outline with
      | `NoOutline -> ()
      | `Edging (r,g,b) ->
        ctx##save;
        ctx##.lineWidth := 3.;
        ctx##.fillStyle := Js.string @@ Printf.sprintf "rgb(%d,%d,%d)" r g b;
        ctx##strokeText (Js.string text) (float_of_int x) (float_of_int y);
        ctx##restore;
      );
      ctx##fillText (Js.string text) (float_of_int x) (float_of_int y);
      ctx##restore
    end
  in
  { id; render; is_visible; x; y; w; h }
