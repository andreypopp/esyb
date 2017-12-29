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

let findSourceModTime = (spec: BuildSpec.t) => {
  open Run;
  let visit = (path: Fpath.t) =>
    fun
    | Ok(maxTime) => {
        let%bind {Unix.st_mtime: time, _} = Bos.OS.Path.symlink_stat(path);
        Ok(time > maxTime ? time : maxTime);
      }
    | error => error;
  let traverse =
    `Sat(
      path =>
        switch (Fpath.basename(path)) {
        | "node_modules" => Ok(false)
        | "_esy" => Ok(false)
        | "_release" => Ok(false)
        | "_build" => Ok(false)
        | "_install" => Ok(false)
        | base when base.[0] == '.' => Ok(false)
        | _ => Ok(true)
        }
    );
  Result.join(
    Bos.OS.Path.fold(
      ~dotfiles=true,
      ~traverse,
      visit,
      Ok(0.),
      [spec.sourcePath]
    )
  );
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
  let env =
    switch (Bos.OS.Env.var("TERM")) {
    | Some(term) => Astring.String.Map.add("TERM", term, spec.env)
    | None => spec.env
    };
  let%bind commandExec = Sandbox.sandboxExec({allowWrite: sandboxConfig});
  let rec runCommands = commands =>
    switch commands {
    | [] => Ok()
    | [cmd, ...cmds] =>
      switch (commandExec(~env, cmd)) {
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

let build =
    (~buildOnly=true, ~force=false, config: Config.t, spec: BuildSpec.t) => {
  open Run;
  Logs.app(m => m("# ESYB: start %s", spec.id));
  let performBuild = sourceModTime => {
    Logs.app(m => m("# ESYB: building"));
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
    let startTime = Unix.gettimeofday();
    let%bind () =
      withBuildEnv(~commit=! buildOnly, config, spec, runBuildAndInstall);
    let%bind info = {
      let%bind sourceModTime =
        switch (sourceModTime, spec.sourceType) {
        | (None, BuildSpec.Root)
        | (None, BuildSpec.Transient) =>
          let%bind v = findSourceModTime(spec);
          Ok(Some(v));
        | (v, _) => Ok(v)
        };
      Ok(
        BuildInfo.{sourceModTime, timeSpent: Unix.gettimeofday() -. startTime}
      );
    };
    BuildInfo.write(config, spec, info);
  };
  switch (force, spec.sourceType) {
  | (true, _) =>
    Logs.app(m => m("# ESYB: forcing build"));
    performBuild(None);
  | (false, BuildSpec.Transient)
  | (false, BuildSpec.Root) =>
    Logs.app(m => m("# ESYB: checking for staleness"));
    let info = BuildInfo.read(config, spec);
    let prevSourceModTime =
      Option.bind(~f=v => v.BuildInfo.sourceModTime, info);
    let%bind sourceModTime = findSourceModTime(spec);
    switch prevSourceModTime {
    | Some(prevSourceModTime) when sourceModTime > prevSourceModTime =>
      performBuild(Some(sourceModTime))
    | None => performBuild(Some(sourceModTime))
    | Some(_) =>
      Logs.app(m => m("# ESYB: source code is not modified, skipping"));
      ok;
    };
  | (false, BuildSpec.Immutable) =>
    let%bind installPathExists = exists(spec.installPath);
    if (installPathExists) {
      Logs.app(m => m("# ESYB: build exists in store, skipping"));
      ok;
    } else {
      performBuild(None);
    };
  };
};
