let length (text: string )= 
  Uutf.String.fold_utf_8 (fun x _ _ -> x + 1) 0 text

let sub (s:string) (pos:int) (len:int) =
  let buf = Buffer.create len in
  let _ : int = Uutf.String.fold_utf_8 (fun off _ elt ->
      match elt with
      | `Uchar x when off >= pos && off < pos + len ->
        Buffer.add_utf_8_uchar buf x;
        off + 1
      | _ -> off + 1) 0 s in
  let result = Buffer.contents buf in
  Buffer.clear buf;
  result
