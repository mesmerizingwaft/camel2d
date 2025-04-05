module Circle = struct
  let check_inclusion ~x ~y ~radius =
    fun x' y' ->
      let dx = x - x' in
      let dy = y - y' in
      dx * dx + dy * dy < radius * radius
end

module Rectangle = struct
  let check_inclusion ~x ~y ~w ~h =
    fun x' y' ->
      x <= x' && x' <= x + w &&
      y <= y' && y' <= y + h
end

