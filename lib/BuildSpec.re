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
        let items = do JsonRepr.of_yojson(json);
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
  buildDir: Fpath.t
};
