include Rresult;

let ok = Ok();

let join = rr =>
  switch rr {
  | Ok(Ok(v)) => Ok(v)
  | Ok(v) => v
  | Error(msg) => Error(msg)
  };

let (>>) = (a, b) =>
  switch a {
  | Ok () => b()
  | Error(msg) => Error(msg)
  };

module Let_syntax = {
  let bind = (~f, v) =>
    switch v {
    | Ok(v) => f(v)
    | Error(e) => Error(e)
    };
  module Open_on_rhs = {
    let return = v => Ok(v);
  };
};
