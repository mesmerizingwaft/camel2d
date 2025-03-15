open Camel2d_entity_types

let create ~pos ~size ?(is_visible=true) ?(z_index=0) ?(alpha=1.0) id resource_name =
  let (x, y), (w, h) = pos, size in
  let render {x; y; w; h; is_visible; alpha; _} context bucket =
    if is_visible then begin
      let img = Camel2d_resource.fetch_image bucket resource_name in
      Camel2d_resource.Image.render img context ~x ~y ~w ~h ~alpha
    end
  in
  { id; render; is_visible; x; y; w; h; z_index; alpha }
