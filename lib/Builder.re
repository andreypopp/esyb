let relocateSourcePath = (config: Config.t, spec: BuildSpec.t) => {
  let cmd =
    Bos.Cmd.(
      empty
      % config.rsyncCmd
      % "--quiet"
      % "--archive"
      % "--exclude"
      % p(spec.buildPath)
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
       * origPath rather than the origPath itself into destPath, see "man rsync" for
       * details.
       */
      % (Fpath.to_string(spec.sourcePath) ++ "/")
      % p(spec.buildPath)
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
        ~origPrefix=spec.stagePath,
        ~destPrefix=spec.installPath,
        path
      )
    | Unix.S_LNK =>
      rewriteTargetInSymlink(
        ~origPrefix=spec.stagePath,
        ~destPrefix=spec.installPath,
        path
      )
    | _ => Ok()
    };
  let%bind () = traverse(spec.stagePath, relocate);
  let%bind () = Bos.OS.Path.move(spec.stagePath, spec.installPath);
  ok;
};

let relocateBuildPath = (_config: Config.t, spec: BuildSpec.t) => {
  open Run;
  let savedBuild = spec.buildPath / "_build";
  let currentBuild = spec.sourcePath / "_build";
  let backupBuild = spec.sourcePath / "_build.prev";
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
  (start, commit);
};

let doNothing = (_config: Config.t, _spec: BuildSpec.t) => Run.ok;

/**
 * Execute `run` within the build environment for `spec`.
 */
let withBuildEnv = (~commit=false, config: Config.t, spec: BuildSpec.t, run) => {
  open Run;
  let {BuildSpec.sourcePath, installPath, buildPath, stagePath, _} = spec;
  let (rootPath, prepareRootPath, completeRootPath) =
    switch (spec.buildType, spec.sourceType) {
    | (InSource, _) => (buildPath, relocateSourcePath, doNothing)
    | (JbuilderLike, Immutable) => (buildPath, relocateSourcePath, doNothing)
    | (JbuilderLike, Transient) =>
      let (start, commit) = relocateBuildPath(config, spec);
      (sourcePath, start, commit);
    | (JbuilderLike, Root) => (sourcePath, doNothing, doNothing)
    | (OutOfSource, _) => (sourcePath, doNothing, doNothing)
    };
  let%bind sandboxConfig = {
    open Sandbox;
    let regex = (base, segments) => {
      let pat =
        String.concat(Fpath.dir_sep, [Fpath.to_string(base), ...segments]);
      Regex(pat);
    };
    let%bind tempPath = {
      let v = Fpath.v(Bos.OS.Env.opt_var("TMPDIR", ~absent="/tmp"));
      let%bind v = realpath(v);
      Ok(Fpath.to_string(v));
    };
    let allowWriteToSourcePath =
      switch spec.buildType {
      | JbuilderLike => [
          Subpath(Fpath.to_string(sourcePath / "_build")),
          regex(sourcePath, [".*", "[^/]*\\.install"]),
          regex(sourcePath, ["[^/]*\\.install"]),
          regex(sourcePath, [".*", "[^/]*\\.opam"]),
          regex(sourcePath, ["[^/]*\\.opam"]),
          regex(sourcePath, [".*", "jbuild-ignore"])
        ]
      | _ => []
      };
    Ok(
      allowWriteToSourcePath
      @ [
        regex(sourcePath, [".*", "\\.merlin"]),
        regex(sourcePath, ["\\.merlin"]),
        Subpath(Fpath.to_string(buildPath)),
        Subpath(Fpath.to_string(stagePath)),
        Subpath("/private/tmp"),
        Subpath("/tmp"),
        Subpath(tempPath)
      ]
    );
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
    let%bind () = rmdir(installPath);
    let%bind () = mkdir(stagePath);
    let%bind () = mkdir(stagePath / "bin");
    let%bind () = mkdir(stagePath / "lib");
    let%bind () = mkdir(stagePath / "etc");
    let%bind () = mkdir(stagePath / "sbin");
    let%bind () = mkdir(stagePath / "man");
    let%bind () = mkdir(stagePath / "share");
    let%bind () = mkdir(stagePath / "doc");
    let%bind () =
      switch (spec.sourceType, spec.buildType) {
      | (Immutable, _)
      | (_, InSource) =>
        let%bind () = rmdir(buildPath);
        let%bind () = mkdir(buildPath);
        ok;
      | _ =>
        let%bind () = mkdir(buildPath);
        ok;
      };
    let%bind () = prepareRootPath(config, spec);
    let%bind () = mkdir(buildPath / "_esy");
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
      let%bind () = completeRootPath(config, spec);
      ok;
    | error =>
      let%bind () = completeRootPath(config, spec);
      error;
    };
  let%bind _store = Store.create(config.storePath);
  let%bind _localStore = Store.create(config.localStorePath);
  let%bind () = prepare();
  let result = withCwd(rootPath, ~f=run(runCommands));
  let%bind () = finalize(result);
  result;
};

let build = (~buildOnly=true, config, spec) => {
  open Run;
  let runBuildAndInstall = (run, ()) => {
    let {BuildSpec.build, install, _} = spec;
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
