module PathSyntax = struct
  let re =
    Re.(compile (seq [char '%'; group (rep1 alnum); char '%']))

  let render env (path : Fpath.t) =
    let open Result in
    let replace g =
      let name = Re.Group.get g 1 in
      match env name with
      | None -> raise Not_found
      | Some value -> value
    in
    let path = Fpath.to_string path in
    let%bind path = try
      Ok (Re.replace ~all:true re path ~f:replace)
    with
    | Not_found ->
      let msg = Printf.sprintf "unable to render path: %s" path in
      Error (`Msg msg)
    in Fpath.of_string path
end

let bindSpecToConfig (config : Config.t) (spec : BuildSpec.t) =
  let open Result in
  let open BuildSpec in
  let env = function
  | "sandbox" -> Some (Fpath.to_string config.sandboxPath)
  | "store" -> Some (Fpath.to_string config.storePath)
  | _ -> None
  in
  let%bind installDir = PathSyntax.render env spec.installDir in
  let%bind stageDir = PathSyntax.render env spec.stageDir in
  let%bind buildDir = PathSyntax.render env spec.buildDir in
  let%bind sourceDir = PathSyntax.render env spec.sourceDir in
  Ok ({ spec with installDir; stageDir; buildDir; sourceDir })

let rsync origDir destDir =
  let s = Fpath.to_string in
  let cmd = [
    "rsync";
    "--quiet"; "--archive";
    "--exclude"; s destDir;
    "--exclude"; "node_modules";
    "--exclude"; "_build";
    "--exclude"; "_release";
    "--exclude"; "_esybuild";
    "--exclude"; "_esyinstall";
    (s origDir) ^ "/"; s destDir
  ] in
  let cmd = Bos.Cmd.of_list cmd in
  Bos.OS.Cmd.run cmd

let build (spec : BuildSpec.t) =
  let open Run in
  let open BuildSpec in
  let config = {
    Config.
    sandboxPath = v ".";
    storePath = v "esystore";
    localStorePath = v "." / "node_modules" / ".cache" / "esystore";
  } in

  let%bind spec = bindSpecToConfig config spec in

  let relocateSources () =
    rsync spec.sourceDir spec.buildDir
  in

  let relocateBuildDir () = ok in

  let relocateBuildDirCleanup () = ok in

  let doNothing () = ok in

  let { sourceDir; installDir; buildDir; stageDir; _ } = spec in

  let rootDir, prepareRootDir, completeRootDir =
    match spec.buildType, spec.sourceType with
    | InSource, _ -> buildDir, relocateSources, doNothing
    | JbuilderLike, Immutable -> buildDir, relocateSources, doNothing
    | JbuilderLike, Transient -> sourceDir, relocateBuildDir, relocateBuildDirCleanup
    | JbuilderLike, Root -> sourceDir, doNothing, doNothing
    | OutOfSource, _ -> sourceDir, doNothing, doNothing
  in

  let sandboxConfig = Sandbox.(
    let allowWriteToSourceDir = match spec.buildType with
    | JbuilderLike -> [
        Subpath (sourceDir / "_build");
        Regex (sourceDir / ".*" / "[^/]*\\.install");
        Regex (sourceDir / "[^/]*\\.install");
        Regex (sourceDir / ".*" / "[^/]*\\.opam");
        Regex (sourceDir / "[^/]*\\.opam");
        Regex (sourceDir / ".*" / "jbuild-ignore");
      ]
    | _ -> []
    in
    allowWriteToSourceDir @ [
      Regex (sourceDir / ".*" / "\\.merlin");
      Regex (sourceDir / "\\.merlin");
      Subpath buildDir;
      Subpath stageDir;
    ]
  ) in

  let%bind commandExec =
    Sandbox.sandboxExec { allowWrite = sandboxConfig }
  in

  let rec runCommands env commands =
    match commands with
    | [] -> Ok ()
    | cmd::cmds -> (match commandExec ~env cmd with
      | Ok _ -> runCommands env cmds
      | Error err -> Error err)
  in

  let prepare () =
    let%bind () = mkdir installDir in
    let%bind () = mkdir stageDir in
    let%bind () =
      if
        spec.sourceType = Immutable ||
        spec.buildType = InSource
      then rmdir buildDir
      else ok
    in
    let%bind () = mkdir buildDir in
    let%bind () = mkdir stageDir in
    let%bind () = mkdir (stageDir / "bin") in
    let%bind () = mkdir (stageDir / "lib") in
    let%bind () = mkdir (stageDir / "etc") in
    let%bind () = mkdir (stageDir / "sbin") in
    let%bind () = mkdir (stageDir / "man") in
    let%bind () = mkdir (stageDir / "share") in
    let%bind () = mkdir (stageDir / "doc") in

    let%bind () = prepareRootDir () in

    let%bind () = mkdir (buildDir / "_esy") in
    let%bind () = mkdir (buildDir / "_esy" / "log") in
    ok
  in

  let%bind store = Store.create config.storePath in
  let%bind localStore = Store.create config.localStorePath in

  let runCommands () =
    let%bind () = runCommands spec.env spec.build in
    let%bind () = runCommands spec.env spec.install in
    ok
  in

  let%bind () = prepare () in
  let%bind () = withCwd rootDir runCommands

  in ok
