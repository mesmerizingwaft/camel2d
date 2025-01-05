
let load_imgs ?(bucket_opt=None) img_root imgs =
  let open Promise in
  let bucket = Option.value
    bucket_opt ~default:(Camel2d_resource.create_bucket ()) in
  List.sequence @@ List.map (fun (key, path) ->
    Camel2d_resource.load_img (img_root ^ path) >>= fun img ->
    Hashtbl.add bucket key img;
    return ()
  ) imgs >>= fun _ -> return bucket
