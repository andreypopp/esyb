/**
 * This implements simple substitution syntax %name%.
 */
let re = Re.(compile(seq([char('%'), group(rep1(alnum)), char('%')])));

type env = string => option(string);

let render = (env: env, path: Fpath.t) => {
  open Result;
  let replace = g => {
    let name = Re.Group.get(g, 1);
    switch (env(name)) {
    | None => raise(Not_found)
    | Some(value) => value
    };
  };
  let path = Fpath.to_string(path);
  let%bind path =
    try (Ok(Re.replace(~all=true, re, path, ~f=replace))) {
    | Not_found =>
      let msg = Printf.sprintf("unable to render path: %s", path);
      Error(`Msg(msg));
    };
  Fpath.of_string(path);
};
