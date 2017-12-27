let orDefault = default =>
  fun
  | None => default
  | Some(v) => v;
