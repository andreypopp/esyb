let relocateSourceDir = (config: Config.t, spec: BuildSpec.t) => {
  let cmd =
    Bos.Cmd.(
      empty
      % config.rsyncCmd
      % "--quiet"
      % "--archive"
      % "--exclude"
      % p(spec.buildDir)
      % "--exclude"
      % "node_modules"
      % "--exclude"
      % "_build"
      % "--exclude"
      % "_release"
      % "--exclude"
      % "_esybuild"
      % "--exclude"
      % "_esyinstall"
      /* The trailing "/" is important as it makes rsync to sync the contents of
       * origDir rather than the origDir itself into destDir, see "man rsync" for
       * details.
       */
      % (Fpath.to_string(spec.sourceDir) ++ "/")
      % p(spec.buildDir)
    );
  Bos.OS.Cmd.run(cmd);
};

let commitBuildToStore = (config: Config.t, spec: BuildSpec.t) => {
  open Run;
  let rewritePrefixInFile = (~origPrefix, ~destPrefix, path) => {
    let cmd =
      Bos.Cmd.(
        empty
        % config.fastreplacestringCmd
        % p(path)
        % p(origPrefix)
        % p(destPrefix)
      );
    Bos.OS.Cmd.run(cmd);
  };
  let rewriteTargetInSymlink = (~origPrefix, ~destPrefix, path) => {
    let%bind targetPath = symlink_target(path);
    switch (Fpath.rem_prefix(origPrefix, targetPath)) {
    | Some(basePath) =>
      let nextTargetPath = Fpath.append(destPrefix, basePath);
      let%bind () = rm(path);
      let%bind () = symlink(~target=nextTargetPath, path);
      ok;
    | None => ok
    };
  };
  let relocate = (path: Fpath.t, stats: Unix.stats) =>
    switch stats.st_kind {
    | Unix.S_REG =>
      rewritePrefixInFile(
        ~origPrefix=spec.stageDir,
        ~destPrefix=spec.installDir,
        path
      )
    | Unix.S_LNK =>
      rewriteTargetInSymlink(
        ~origPrefix=spec.stageDir,
        ~destPrefix=spec.installDir,
        path
      )
    | _ => Ok()
    };
  let%bind () = traverse(spec.stageDir, relocate);
  let%bind () = Bos.OS.Path.move(spec.stageDir, spec.installDir);
  ok;
};

let relocateBuildDir = (config: Config.t, spec: BuildSpec.t) => {
  open Run;
  let savedBuild = spec.buildDir / "_build";
  let currentBuild = spec.sourceDir / "_build";
  let backupBuild = spec.sourceDir / "_build.prev";
  let start = (_config, _spec) => {
    let%bind () =
      if%bind (exists(currentBuild)) {
        mv(currentBuild, backupBuild);
      } else {
        ok;
      };
    let%bind () = mkdir(savedBuild);
    let%bind () = mv(savedBuild, currentBuild);
    ok;
  };
  let commit = (_config, _spec) => {
    let%bind () =
      if%bind (exists(currentBuild)) {
        mv(currentBuild, savedBuild);
      } else {
        ok;
      };
    let%bind () =
      if%bind (exists(backupBuild)) {
        mv(backupBuild, currentBuild);
      } else {
        ok;
      };
    ok;
  };
  /*
   # save _build
   if [ -d "$esy_build__source_root/_build" ]; then
     mv "$esy_build__source_root/_build" "$cur__target_dir/_build"
   fi

   # restore original _build
   if [ -d "$cur__target_dir/_build.prev" ]; then
     mv "$cur__target_dir/_build.prev" "$esy_build__source_root/_build"
   fi
   */
  (start, commit);
};

let relocateBuildDirCleanup = (config: Config.t, _spec: BuildSpec.t) => Run.ok;

let doNothing = (_config: Config.t, _spec: BuildSpec.t) => Run.ok;

/**
 * Execute `run` within the build environment for `spec`.
 */
let withBuildEnv = (~commit=false, config: Config.t, spec: BuildSpec.t, run) => {
  open Run;
  let {BuildSpec.sourceDir, installDir, buildDir, stageDir, _} = spec;
  let (rootDir, prepareRootDir, completeRootDir) =
    switch (spec.buildType, spec.sourceType) {
    | (InSource, _) => (buildDir, relocateSourceDir, doNothing)
    | (JbuilderLike, Immutable) => (buildDir, relocateSourceDir, doNothing)
    | (JbuilderLike, Transient) =>
      let (start, commit) = relocateBuildDir(config, spec);
      (sourceDir, start, commit);
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
  let rec runCommands = commands =>
    switch commands {
    | [] => Ok()
    | [cmd, ...cmds] =>
      switch (commandExec(~env=spec.env, cmd)) {
      | Ok(_) => runCommands(cmds)
      | Error(err) => Error(err)
      }
    };
  /*
   * Prepare build/install.
   */
  let prepare = () => {
    let%bind () = rmdir(installDir);
    let%bind () = mkdir(stageDir);
    let%bind () = mkdir(stageDir / "bin");
    let%bind () = mkdir(stageDir / "lib");
    let%bind () = mkdir(stageDir / "etc");
    let%bind () = mkdir(stageDir / "sbin");
    let%bind () = mkdir(stageDir / "man");
    let%bind () = mkdir(stageDir / "share");
    let%bind () = mkdir(stageDir / "doc");
    let%bind () =
      if (spec.sourceType == Immutable || spec.buildType == InSource) {
        let%bind () = rmdir(buildDir);
        let%bind () = mkdir(buildDir);
        ok;
      } else {
        let%bind () = mkdir(buildDir);
        ok;
      };
    let%bind () = prepareRootDir(config, spec);
    let%bind () = mkdir(buildDir / "_esy");
    let%bind () = mkdir(buildDir / "_esy" / "log");
    ok;
  };
  /*
   * Finalize build/install.
   */
  let finalize = result =>
    switch result {
    | Ok () =>
      let%bind () =
        if (commit) {
          commitBuildToStore(config, spec);
        } else {
          ok;
        };
      let%bind () = completeRootDir(config, spec);
      ok;
    | error =>
      let%bind () = completeRootDir(config, spec);
      error;
    };
  let%bind store = Store.create(config.storePath);
  let%bind localStore = Store.create(config.localStorePath);
  let%bind () = prepare();
  let result = withCwd(rootDir, run(runCommands));
  let%bind () = finalize(result);
  result;
};

let build = (~buildOnly=true, config, spec) => {
  open Run;
  let runBuildAndInstall = (run, ()) => {
    let {BuildSpec.build, install} = spec;
    let%bind () = run(build);
    let%bind () =
      if (! buildOnly) {
        run(install);
      } else {
        ok;
      };
    ok;
  };
  withBuildEnv(~commit=! buildOnly, config, spec, runBuildAndInstall);
};
