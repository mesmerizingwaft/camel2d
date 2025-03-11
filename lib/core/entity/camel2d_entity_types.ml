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
  z_index: int;
}
