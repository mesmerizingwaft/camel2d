type t = {
  cho: string;
  han: string;
  speech: string;
  dice_face: string array;
  win: string;
  lose: string;
  message: string;
} [@@deriving yojson]

let t =
  Yojson.Safe.from_string [%blob "literals/ja.json"]
  |> of_yojson
  |> Result.get_ok
