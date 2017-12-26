include Rresult

let ok =
  Ok ()

let join rr =
  match rr with
  | Ok (Ok v) -> Ok v
  | Ok v -> v
  | Error msg -> Error msg

let (>>) a b = match a with
  | Ok () -> b ()
  | Error msg -> Error msg

module Let_syntax = struct

  let bind ~f  v = match v with
    | Ok v -> f v
    | Error e -> Error e

  module Open_on_rhs = struct
    let return v = Ok v
  end

end
