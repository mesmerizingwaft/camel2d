open Camel2d
open Entity

include TextLabel

let create
  ~style
  ~pos
  ~f
  ?(n=10)
  ?(base_horizontal=BHLeft)
  id text =
    let range = List.of_seq @@ Seq.take n (Seq.ints 0) in
    let init context =
      let labels = List.map (fun t ->
        let id = id ^ "_" ^ string_of_int t in
        let pt = style.pt + f t in
        let style = { style with pt } in
        let is_visible = false in
        create ~context ~style ~pos ~is_visible ~base_horizontal id text
      ) range in
      let open World in
      spawn labels
    in
    let updator =
      let open World in
      let cand_ids = List.map (fun t -> id ^ "_" ^ string_of_int t) range in
      let is_cand = Condition.(any_of (List.map has_id cand_ids)) in
      let* is_not_init = exists Condition.(is_cand &&& visible) in
      if not is_not_init then begin
        let id = id ^ "_0" in
        update_when Condition.(has_id id) Updator.show
        >> to_front id
      end
      else
        let* target_id = id_of Condition.(is_cand &&& visible) in
        let target_no =
          String.split_on_char '_' target_id
          |> List.rev
          |> List.hd
          |> int_of_string
        in
        if target_no < (n - 1) then
          let next_no = target_no + 1 in
          let target_id = id ^ "_" ^ string_of_int next_no in
          let is_target = Condition.has_id target_id in
          update_when is_cand Updator.hide
          >> update_when is_target Updator.show
          >> to_front target_id
        else
          return ()
    in
    init, updator
