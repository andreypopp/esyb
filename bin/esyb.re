/**
 * Add monadic syntax handlers for Result.
 */
module Result = {
  include Rresult;
  module Let_syntax = {
    let bind = (~f, v) =>
      switch v {
      | Ok(v) => f(v)
      | Error(e) => Error(e)
      };
    module Open_on_rhs = {
      let return = (v) => Ok(v);
    };
  };
};

let listdir = (path) => {
  open Result;
  print_endline("Directory: " ++ path);
  let files = do Bos.OS.Dir.contents(Fpath.v(path));
  let lines =
    files |> List.map((file) => Fpath.to_string(file)) |> String.concat("\n");
  print_endline(lines);
  Ok();
};

let main = () =>
  Result.(
    {
      do listdir(".");
      do listdir("node_modules");
      Ok();
    }
  );

main();
