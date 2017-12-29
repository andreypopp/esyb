/**
 * Information about the build.
 */
[@deriving (show, of_yojson, to_yojson)]
type t = {
  timeSpent: float,
  sourceModTime: option(float)
};

let path = (config: Config.t, build: BuildSpec.t) => {
  let storeBuildTree = Fpath.parent(build.buildPath);
  Fpath.(storeBuildTree / (build.id ++ ".info"));
};

let write = (config: Config.t, build: BuildSpec.t, info: t) => {
  let write = (oc, ()) => {
    Yojson.Safe.pretty_to_channel(oc, to_yojson(info));
    Run.ok;
  };
  let path = path(config, build);
  Result.join(Bos.OS.File.with_oc(path, write, ()));
};

let read = (config: Config.t, build: BuildSpec.t) => {
  let read = {
    open Run;
    let path = path(config, build);
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
