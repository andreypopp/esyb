[@deriving show]
type sourceType =
  | Immutable
  | Transient
  | Root;

let sourceType_of_yojson = (json: Yojson.Safe.json) =>
  switch json {
  | `String("immutable") => Ok(Immutable)
  | `String("transient") => Ok(Transient)
  | `String("root") => Ok(Root)
  | _ => Error("invalid buildType")
  };

[@deriving show]
type buildType =
  | InSource
  | JbuilderLike
  | OutOfSource;

let buildType_of_yojson = (json: Yojson.Safe.json) =>
  switch json {
  | `String("in-source") => Ok(InSource)
  | `String("out-of-source") => Ok(OutOfSource)
  | `String("_build") => Ok(JbuilderLike)
  | _ => Error("invalid buildType")
  };

module Cmd = {
  module JsonRepr = {
    [@deriving of_yojson]
    type t = list(string);
  };
  type t = Bos.Cmd.t;
  let pp = Bos.Cmd.pp;
  let of_yojson = (json: Yojson.Safe.json) =>
    Result.(
      {
        let%bind items = JsonRepr.of_yojson(json);
        Ok(Bos.Cmd.of_list(items));
      }
    );
};

module Fpath = {
  include Fpath;
  let of_yojson = (json: Yojson.Safe.json) =>
    switch json {
    | `String(v) =>
      switch (Fpath.of_string(v)) {
      | Ok(v) => Ok(v)
      | Error(`Msg(msg)) => Error(msg)
      }
    | _ => Error("invalid path")
    };
};

module Env = {
  type t = Bos.OS.Env.t;
  let pp = (_fmt, _env) => ();
  let of_yojson = (json: Yojson.Safe.json) =>
    switch json {
    | `Assoc(items) =>
      let add_to_map = (res, (key, value)) =>
        switch (res, value) {
        | (Ok(res), `String(value)) =>
          Ok(Astring.String.Map.add(key, value, res))
        | _ => Error("expected a string value")
        };
      List.fold_left(add_to_map, Ok(Astring.String.Map.empty), items);
    | _ => Error("expected an object")
    };
};

[@deriving (show, of_yojson)]
type t = {
  id: string,
  name: string,
  version: string,
  sourceType,
  buildType,
  build: list(Cmd.t),
  install: list(Cmd.t),
  sourceDir: Fpath.t,
  stageDir: Fpath.t,
  installDir: Fpath.t,
  buildDir: Fpath.t,
  env: Env.t
};

let bindSpecToConfig = (config: Config.t, spec: t) => {
  open Result;
  let env =
    fun
    | "sandbox" => Some(Fpath.to_string(config.sandboxPath))
    | "store" => Some(Fpath.to_string(config.storePath))
    | "localStore" => Some(Fpath.to_string(config.localStorePath))
    | _ => None;
  let%bind installDir = PathSyntax.render(env, spec.installDir);
  let%bind stageDir = PathSyntax.render(env, spec.stageDir);
  let%bind buildDir = PathSyntax.render(env, spec.buildDir);
  let%bind sourceDir = PathSyntax.render(env, spec.sourceDir);
  Ok({...spec, installDir, stageDir, buildDir, sourceDir});
};

let ofFile = (config: Config.t, path: Fpath.t) =>
  Run.(
    {
      let%bind data = Bos.OS.File.read(path);
      let%bind spec = Json.parseWith(of_yojson, data);
      let%bind spec = bindSpecToConfig(config, spec);
      Ok(spec);
    }
  );
