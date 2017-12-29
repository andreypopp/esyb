/**
 * Information about the spec.
 */
[@deriving (show, of_yojson, to_yojson)]
type t = {
  timeSpent: float,
  sourceModTime: option(float)
};

let path = (spec: BuildSpec.t) => {
  let storeBuildTree = Fpath.parent(spec.buildPath);
  Fpath.(storeBuildTree / (spec.id ++ ".info"));
};

let write = (spec: BuildSpec.t, info: t) => {
  let write = (oc, ()) => {
    Yojson.Safe.pretty_to_channel(oc, to_yojson(info));
    Run.ok;
  };
  let path = path(spec);
  Result.join(Bos.OS.File.with_oc(path, write, ()));
};

let read = (spec: BuildSpec.t) => {
  let read = {
    open Run;
    let path = path(spec);
    if%bind (exists(path)) {
      let%bind data = Bos.OS.File.read(path);
      let%bind info = Json.parseWith(of_yojson, data);
      Ok(Some(info));
    } else {
      Ok(None);
    };
  };
  switch read {
  | Ok(v) => v
  | Error(_) => None
  };
};
