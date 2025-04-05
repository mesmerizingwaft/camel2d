(* gif.ml *)

exception InvalidGIF of string

let invalid_gif component actual expected =
  let msg = Printf.sprintf "Actual: 0x%x, Expected: 0x%x @%s" actual expected component in
  raise (InvalidGIF msg)


module LogicalScreenDescriptor: sig
  type t

  val parse : bytes -> t * bytes
  val has_global_color_table : t -> bool
  val global_color_table_size_of : t -> int option
  val print : t -> unit
  val to_bytes: t -> bytes
end = struct
  type t = {
    width: int;
    height: int;
    packed_field: int;
    background_color_index: int;
    pixel_aspect_ratio: int;
  }

  let parse data =
    let width = Bytes.get_uint16_le data 0 in
    let height = Bytes.get_uint16_le data 2 in
    let packed_field = Bytes.get_uint8 data 4 in
    (*
    let has_global_color_table = (packed land 0x80) <> 0 in
    let color_resolution = ((packed lsr 4) land 0x07) + 1 in
    let sorted = (packed land 0x08) <> 0 in
    let global_color_table_size = 1 lsl ((packed land 0x07) + 1) in
    *)
    let background_color_index = Bytes.get_uint8 data 5 in
    let pixel_aspect_ratio = Bytes.get_uint8 data 6 in
    ({ width; height; packed_field; background_color_index; pixel_aspect_ratio }, Bytes.sub data 7 (Bytes.length data - 7))

  let has_global_color_table t =
    (t.packed_field land 0x80) <> 0

  let global_color_table_size_of t =
    if has_global_color_table t then
      Some (1 lsl ((t.packed_field land 0x07) + 1))
    else
      None

  let print t =
    Printf.printf "Width: %d\n" t.width;
    Printf.printf "Height: %d\n" t.height;
    Printf.printf "Has Global Color Table: %b\n" (has_global_color_table t);
    Printf.printf "Background Color Index: %d\n" t.background_color_index;
    Printf.printf "Pixel Aspect Ratio: %d\n" t.pixel_aspect_ratio

  let to_bytes t =
    let data = Bytes.create 7 in
    Bytes.set_uint16_le data 0 t.width;
    Bytes.set_uint16_le data 2 t.height;
    Bytes.set_uint8 data 4 t.packed_field;
    Bytes.set_uint8 data 5 t.background_color_index;
    Bytes.set_uint8 data 6 t.pixel_aspect_ratio;
    data
end

module GlobalColorTable: sig
  type t
  val parse : int -> bytes -> t * bytes
  val to_bytes : t -> bytes
end = struct
  type t = bytes

  let parse size data =
    let gct = Bytes.sub data 0 (3 * size) in
    let rest = Bytes.sub data (3 * size) (Bytes.length data - (3 * size)) in
    (gct, rest)
  let to_bytes t =
    let size = Bytes.length t in
    if size mod 3 <> 0 then
      failwith "Invalid Global Color Table size"
    else
      let gct = Bytes.create size in
      Bytes.blit t 0 gct 0 size;
      gct
end

module GraphicControlExtension: sig
  type t
  val parse : bytes -> t * bytes
  val to_bytes : t -> bytes
  val delay_time_of : t -> int
end = struct
  type t = {
    packed_field: int;
    delay_time: int;
    transparent_color_index: int;
  }

  let parse data =
    if not (Bytes.get data 0 = '\x21')
    then invalid_gif "GraphicControlExtension" (Bytes.get_int8 data 0) 0x21
    else if not (Bytes.get data 1 = '\xF9')
    then invalid_gif "GraphicControlExtension" (Bytes.get_int8 data 1) 0xF9
    else
      let _byte_size = Bytes.get_uint8 data 2 in
      let packed_field = Bytes.get_uint8 data 3 in
      let delay_time = Bytes.get_uint16_le data 4 in
      let transparent_color_index = Bytes.get_uint8 data 6 in
      let _block_terminator = Bytes.get_uint8 data 7 in
      let data = Bytes.sub data 8 (Bytes.length data - 8) in
      { packed_field; delay_time; transparent_color_index },
      data

  let to_bytes t =
    let data = Bytes.create 8 in
    Bytes.set data 0 '\x21';
    Bytes.set data 1 '\xF9';
    Bytes.set_uint8 data 2 4;
    Bytes.set_uint8 data 3 t.packed_field;
    Bytes.set_uint16_le data 4 t.delay_time;
    Bytes.set_uint8 data 6 t.transparent_color_index;
    Bytes.set_uint8 data 7 0;
    data

  let delay_time_of t = t.delay_time
end

module ApplicationExtension: sig
  type t
  val parse : bytes -> t * bytes
  val to_bytes: t -> bytes
end = struct
  type t = {
    application_identifier: bytes;
    application_auth_code: bytes;
    application_sub_block: bytes;
  }

  let parse data =
    if not (Bytes.get data 0 = '\x21')
    then invalid_gif "ApplicationExtension" (Bytes.get_int8 data 0) 0x21
    else if not (Bytes.get data 1 = '\xFF')
    then invalid_gif "ApplicationExtension" (Bytes.get_int8 data 1) 0xFF
    else
      let _block_size = Bytes.get_uint8 data 2 in
      let application_identifier = Bytes.sub data 3 8 in
      let application_auth_code = Bytes.sub data 11 3 in
      let application_sub_block = Bytes.sub data 14 5 in
      let data = Bytes.sub data 19 (Bytes.length data - 19) in
      { application_identifier; application_auth_code; application_sub_block },
      data

  let to_bytes t =
    let data = Bytes.create 19 in
    Bytes.set data 0 '\x21';
    Bytes.set data 1 '\xFF';
    Bytes.set_uint8 data 2 11;
    Bytes.blit t.application_identifier 0 data 3 8;
    Bytes.blit t.application_auth_code 0 data 11 3;
    Bytes.blit t.application_sub_block 0 data 14 5;
    data
end

module ImageDescriptor: sig
  type t
  val parse : bytes -> t * bytes
  val size_of_local_color_table : t -> int option
  val to_bytes: t -> bytes
end = struct
  type t = {
    left_position: int;
    top_position: int;
    width: int;
    height: int;
    packed_field: int;
  }

  let parse data =
    if not (Bytes.get data 0 = '\x2C')
    then invalid_gif "ImageDescriptor" (Bytes.get_int8 data 0) 0x2C
    else
      let left_position = Bytes.get_uint16_le data 1 in
      let top_position = Bytes.get_uint16_le data 3 in
      let width = Bytes.get_uint16_le data 5 in
      let height = Bytes.get_uint16_le data 7 in
      let packed_field = Bytes.get_uint8 data 9 in
      ({ left_position; top_position; width; height; packed_field },
      Bytes.sub data 10 (Bytes.length data - 10))

  let size_of_local_color_table t =
    let packed_field = t.packed_field in
    if (packed_field land 0x80) <> 0 then
      Some (1 lsl ((packed_field land 0x07) + 1))
    else
      None

  let to_bytes t =
    let data = Bytes.create 10 in
    Bytes.set data 0 '\x2C';
    Bytes.set_uint16_le data 1 t.left_position;
    Bytes.set_uint16_le data 3 t.top_position;
    Bytes.set_uint16_le data 5 t.width;
    Bytes.set_uint16_le data 7 t.height;
    Bytes.set_uint8 data 9 t.packed_field;
    data
end

module LocalColorTable: sig
  type t
  val parse: int -> bytes -> t * bytes
  val to_bytes: t -> bytes
end = struct
  type t = bytes

  let parse size data =
    let lct = Bytes.sub data 0 (3 * size) in
    let rest = Bytes.sub data (3 * size) (Bytes.length data - (3 * size)) in
    (lct, rest)
  let to_bytes t =
    let size = Bytes.length t in
    if size mod 3 <> 0 then
      failwith "Invalid Local Color Table size"
    else
      let lct = Bytes.create size in
      Bytes.blit t 0 lct 0 size;
      lct
end

module ImageData : sig
  type t
  val parse: bytes -> t * bytes
  val to_bytes: t -> bytes
end = struct
  type t = {
    lzw_minimum_code_size: int;
    data_sub_blocks: bytes list;
  }

  let parse data =
    let lzw_minimum_code_size = Bytes.get_uint8 data 0 in
    let rec data_sub_blocks blocks data =
      if Bytes.get_uint8 data 0 = 0 then
        List.rev blocks, Bytes.sub data 1 (Bytes.length data - 1)
      else
        let size = Bytes.get_uint8 data 0 in
        let sub_block = Bytes.sub data 1 size in
        let rest = Bytes.sub data (1 + size) (Bytes.length data - (1 + size)) in
        data_sub_blocks (sub_block :: blocks) rest
    in
    let blocks, rest = data_sub_blocks [] (Bytes.sub data 1 (Bytes.length data - 1)) in
    ({ lzw_minimum_code_size; data_sub_blocks = blocks }, rest)
  let to_bytes t =
    let data = Bytes.create 1 in
    Bytes.set_uint8 data 0 t.lzw_minimum_code_size;
    let blocks = List.map (fun block ->
      let size = Bytes.length block in
      let sub_block = Bytes.create (size + 1) in
      Bytes.set_uint8 sub_block 0 size;
      Bytes.blit block 0 sub_block 1 size;
      sub_block) t.data_sub_blocks in
    let block_terminator = Bytes.create 1 in
    Bytes.set_uint8 block_terminator 0 0;
    let data = Bytes.concat Bytes.empty (data :: blocks @ [block_terminator]) in
    data
end

type block =
  | GraphicControlExtension of GraphicControlExtension.t
  | ApplicationExtension of ApplicationExtension.t
  | ImageDescriptor of ImageDescriptor.t
  | LocalColorTable of LocalColorTable.t
  | ImageData of ImageData.t

type t = {
  header: string;
  logical_screen_descriptor: LogicalScreenDescriptor.t;
  global_color_table: GlobalColorTable.t option;
  blocks: block list;
}

let parse_header (data: bytes) =
  if Bytes.length data < 6 then raise (InvalidGIF "header");
  let header = Bytes.sub_string data 0 6 in
  if header <> "GIF87a" && header <> "GIF89a" then raise (InvalidGIF "header");
  header, Bytes.sub data 6 (Bytes.length data - 6)

let extension_introducer = 0x21
let image_separator = 0x2C
let trailer = 0x3B
let is_extension (data: bytes): bool =
  Bytes.get_int8 data 0 = extension_introducer

let is_image_descriptor (data: bytes): bool =
  Bytes.get_int8 data 0 = image_separator
let is_trailer (data: bytes): bool =
  Bytes.get_int8 data 0 = trailer

let parse_blocks (data: bytes): block list =
  let rec parse blocks data =
    if is_extension data then parse_extension blocks data
    else if is_image_descriptor data then parse_image_descriptor blocks data
    else if is_trailer data then begin
      List.rev blocks
    end
    else raise (InvalidGIF "illegal block")
  and parse_extension blocks data =
    match Bytes.get data 1 with
    | '\xF9' ->
      let gce, rest = GraphicControlExtension.parse data in
      parse (GraphicControlExtension gce :: blocks) rest
    | '\xFF' ->
      let app_ext, rest = ApplicationExtension.parse data in
      parse (ApplicationExtension app_ext :: blocks) rest
    | '\xFE' ->
      failwith "unimplemented: CommentExtension"
    | _ -> raise (InvalidGIF "unknown extension")
  and parse_image_descriptor blocks data =
    let id, data = ImageDescriptor.parse data in
    let blocks, data =
      match ImageDescriptor.size_of_local_color_table id with
      | None ->
        ((ImageDescriptor id)::blocks), data
      | Some size ->
        let lt, data = LocalColorTable.parse size data in
        ((LocalColorTable lt)::(ImageDescriptor id)::blocks), data
    in
    let image_data, data = ImageData.parse data in
    parse (ImageData image_data::blocks) data
  in
  parse [] data


let from_bytes (data: bytes): t =
  let header, data = parse_header data in
  let logical_screen_descriptor, data = LogicalScreenDescriptor.parse data in
  let gct_size = LogicalScreenDescriptor.global_color_table_size_of logical_screen_descriptor in
  let global_color_table, data =
    match gct_size with
    | Some size ->
      let gct, rest = GlobalColorTable.parse size data in
      Some gct, rest
    | None -> None, data
  in
  let blocks = parse_blocks data in
  { header; logical_screen_descriptor; global_color_table; blocks }

let to_bytes (gif: t): bytes =
  let header = Bytes.create 6 in
  Bytes.blit_string gif.header 0 header 0 6;
  let logical_screen_descriptor = LogicalScreenDescriptor.to_bytes gif.logical_screen_descriptor in
  let global_color_table =
    match gif.global_color_table with
    | Some gct -> GlobalColorTable.to_bytes gct
    | None -> Bytes.empty
  in
  let blocks = List.map (function
    | GraphicControlExtension gce -> GraphicControlExtension.to_bytes gce
    | ApplicationExtension app_ext -> ApplicationExtension.to_bytes app_ext
    | ImageDescriptor id -> ImageDescriptor.to_bytes id
    | LocalColorTable lct -> LocalColorTable.to_bytes lct
    | ImageData id -> ImageData.to_bytes id) gif.blocks in
  let _trailer = Bytes.create 1 in
  Bytes.set_uint8 _trailer 0 trailer;
  Bytes.concat Bytes.empty (header :: logical_screen_descriptor :: global_color_table :: blocks @ [_trailer])


let slice_animated_gif (gif:t): t list =
  let common_header = gif.header in
  let common_logical_screen_descriptor = gif.logical_screen_descriptor in
  let common_global_color_table = gif.global_color_table in
  let rec inner gifs blocks =
    match blocks with
      | [] -> gifs
      | GraphicControlExtension gce :: ImageDescriptor id :: LocalColorTable lct :: ImageData idata :: rest ->
        let gce = GraphicControlExtension gce in
        let id = ImageDescriptor id in
        let lct = LocalColorTable lct in
        let idata = ImageData idata in
        let gif = { header=common_header; logical_screen_descriptor=common_logical_screen_descriptor; global_color_table=common_global_color_table; blocks=[gce; id; lct; idata] } in
        inner (gif::gifs) rest
      | GraphicControlExtension gce :: ImageDescriptor id :: ImageData idata :: rest ->
        let gce = GraphicControlExtension gce in
        let id = ImageDescriptor id in
        let idata = ImageData idata in
        let gif = { header=common_header; logical_screen_descriptor=common_logical_screen_descriptor; global_color_table=common_global_color_table; blocks=[gce; id; idata] } in
        inner (gif::gifs) rest
      | _::blocks -> inner gifs blocks
  in
inner [] gif.blocks

let delay_time_of (frame: t) =
  let rec inner blocks =
    match blocks with
      | GraphicControlExtension gce :: _ -> GraphicControlExtension.delay_time_of gce
      | _::blocks -> inner blocks
      | [] -> failwith "no GraphicControlExtension"
  in
  inner frame.blocks

(* Example usage *)
(*
let _ =
  let ic = open_in_bin "./samples/popping_alfie/static/imgs/alfie.gif" in
  let len = in_channel_length ic in
  let data = Bytes.create len in
  really_input ic data 0 len;
  close_in ic;
  let gif = from_bytes data in
  let gifs = slice_animated_gif gif in
  List.iteri (fun i gif ->
    let bytes = to_bytes gif in
    let oc = open_out_bin (Printf.sprintf "./dump%d.gif" i) in
    output_bytes oc bytes;
    close_out oc;
  ) gifs;
  (*let _ = from_bytes (to_bytes gif) in*)
  (*
  try
    assert (to_bytes gif = data)
  with e ->
    let oc = open_out_bin "./dump1.bin" in
    output_bytes oc data;
    close_out oc;
    let oc = open_out_bin "./dump2.bin" in
    output_bytes oc (to_bytes gif);
    close_out oc;
    raise e
    *)*)