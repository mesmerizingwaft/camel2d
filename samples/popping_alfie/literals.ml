type t = {
  title: string;
  title_inst: string;
  tweet_body: string;
  url: string;
  label_tweet: string;
  label_retry: string;
} [@@deriving yojson]

let t =
  Yojson.Safe.from_string [%blob "literals/ja.json"]
  |> of_yojson
  |> Result.get_ok
