module File = Bos.OS.File;

module Dir = Bos.OS.Dir;

module Store = {
  type t = {path: Fpath.t};
  let create = (path: Fpath.t) =>
    Result.(
      {
        let _ = do Dir.create(Fpath.(path / "i"));
        let _ = do Dir.create(Fpath.(path / "b"));
        let _ = do Dir.create(Fpath.(path / "s"));
        Ok({path: path});
      }
    );
};

module Config = {
  type t = {
    sandboxPath: Fpath.t,
    storePath: Fpath.t
  };
};

let rec runCommands = (commands) =>
  switch commands {
  | [] => Ok()
  | [cmd, ...cmds] =>
    Logs.app((m) => m("COMMAND: %s", Bos.Cmd.to_string(cmd)));
    switch (Bos.OS.Cmd.run(cmd)) {
    | Ok(_) => runCommands(cmds)
    | Error(err) => Error(err)
    };
  };

let build = (spec: BuildSpec.t) => {
  open Result;
  let config =
    Config.{sandboxPath: Fpath.v("."), storePath: Fpath.v("_store")};
  let rootDir = do
    switch spec.buildType {
    | BuildSpec.InSource => Ok(spec.buildDir)
    | BuildSpec.JbuilderLike => Ok(spec.sourceDir)
    | BuildSpec.OutOfSource => Ok(spec.sourceDir)
    };
  let store = do Store.create(config.storePath);
  do join(
       Bos.OS.Dir.with_current(
         rootDir,
         () => {
           do runCommands(spec.build);
           do runCommands(spec.install);
           Ok();
         },
         ()
       )
     );
  Ok();
};
