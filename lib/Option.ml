
let orDefault default  = function
  | None -> default
  | Some v -> v
