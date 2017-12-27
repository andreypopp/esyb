module PathSyntax = {
  let re = Re.(compile(seq([char('%'), group(rep1(alnum)), char('%')])));
  let render = (env, path: Fpath.t) => {
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
};

let bindSpecToConfig = (config: Config.t, spec: BuildSpec.t) => {
  open Result;
  open BuildSpec;
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

let rsync = (origDir, destDir) => {
  let s = Fpath.to_string;
  let cmd = [
    "rsync",
    "--quiet",
    "--archive",
    "--exclude",
    s(destDir),
    "--exclude",
    "node_modules",
    "--exclude",
    "_build",
    "--exclude",
    "_release",
    "--exclude",
    "_esybuild",
    "--exclude",
    "_esyinstall",
    s(origDir) ++ "/",
    s(destDir)
  ];
  let cmd = Bos.Cmd.of_list(cmd);
  Bos.OS.Cmd.run(cmd);
};

let build = (spec: BuildSpec.t) => {
  open Run;
  open BuildSpec;
  let config = {
    Config.sandboxPath: v("."),
    storePath: v("esystore"),
    localStorePath: v(".") / "node_modules" / ".cache" / "esystore"
  };
  let%bind spec = bindSpecToConfig(config, spec);
  let relocateSources = () => rsync(spec.sourceDir, spec.buildDir);
  let relocateBuildDir = () => ok;
  let relocateBuildDirCleanup = () => ok;
  let doNothing = () => ok;
  let {sourceDir, installDir, buildDir, stageDir, _} = spec;
  let (rootDir, prepareRootDir, completeRootDir) =
    switch (spec.buildType, spec.sourceType) {
    | (InSource, _) => (buildDir, relocateSources, doNothing)
    | (JbuilderLike, Immutable) => (buildDir, relocateSources, doNothing)
    | (JbuilderLike, Transient) => (
        sourceDir,
        relocateBuildDir,
        relocateBuildDirCleanup
      )
    | (JbuilderLike, Root) => (sourceDir, doNothing, doNothing)
    | (OutOfSource, _) => (sourceDir, doNothing, doNothing)
    };
  let sandboxConfig = {
    open Sandbox;
    let allowWriteToSourceDir =
      switch spec.buildType {
      | JbuilderLike => [
          Subpath(sourceDir / "_build"),
          Regex(sourceDir / ".*" / "[^/]*\\.install"),
          Regex(sourceDir / "[^/]*\\.install"),
          Regex(sourceDir / ".*" / "[^/]*\\.opam"),
          Regex(sourceDir / "[^/]*\\.opam"),
          Regex(sourceDir / ".*" / "jbuild-ignore")
        ]
      | _ => []
      };
    allowWriteToSourceDir
    @ [
      Regex(sourceDir / ".*" / "\\.merlin"),
      Regex(sourceDir / "\\.merlin"),
      Subpath(buildDir),
      Subpath(stageDir)
    ];
  };
  let%bind commandExec = Sandbox.sandboxExec({allowWrite: sandboxConfig});
  let rec runCommands = (env, commands) =>
    switch commands {
    | [] => Ok()
    | [cmd, ...cmds] =>
      switch (commandExec(~env, cmd)) {
      | Ok(_) => runCommands(env, cmds)
      | Error(err) => Error(err)
      }
    };
  let prepare = () => {
    let%bind () = mkdir(installDir);
    let%bind () = mkdir(stageDir);
    let%bind () =
      if (spec.sourceType == Immutable || spec.buildType == InSource) {
        rmdir(buildDir);
      } else {
        ok;
      };
    let%bind () = mkdir(buildDir);
    let%bind () = mkdir(stageDir);
    let%bind () = mkdir(stageDir / "bin");
    let%bind () = mkdir(stageDir / "lib");
    let%bind () = mkdir(stageDir / "etc");
    let%bind () = mkdir(stageDir / "sbin");
    let%bind () = mkdir(stageDir / "man");
    let%bind () = mkdir(stageDir / "share");
    let%bind () = mkdir(stageDir / "doc");
    let%bind () = prepareRootDir();
    let%bind () = mkdir(buildDir / "_esy");
    let%bind () = mkdir(buildDir / "_esy" / "log");
    ok;
  };
  let%bind store = Store.create(config.storePath);
  let%bind localStore = Store.create(config.localStorePath);
  let runCommands = () => {
    let%bind () = runCommands(spec.env, spec.build);
    let%bind () = runCommands(spec.env, spec.install);
    ok;
  };
  let%bind () = prepare();
  let%bind () = withCwd(rootDir, runCommands);
  ok;
};
