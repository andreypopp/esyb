open EsyLib

module File = Bos.OS.File
module Dir = Bos.OS.Dir

let runToTerm r =
  match r with
  | Error (`Msg msg) -> `Error (false, msg)
  | _ -> `Ok ()

let build _copts =
  Logs.app (fun m -> m "Hello horrible world!");
  runToTerm Run.(
    let buildPath = v "fixtures" / "simple" / "build.json" in
    let%bind spec = BuildSpec.ofFile buildPath in
    let%bind () = Builder.build spec in
    Ok ()
  )

let shell _copts =
  Logs.set_reporter (Logs_fmt.reporter ());
  runToTerm (Ok ())

let exec _copts =
  Logs.set_reporter (Logs_fmt.reporter ());
  runToTerm (Ok ())

type verb = Normal | Quiet | Verbose

type commonOptions = {
  debug : bool;
  verb : verb;
  prefixPath : string option;
  sandboxPath : string option;
}

let help _copts man_format cmds topic =
  match topic with
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
      let topics = "topics" :: "patterns" :: "environment" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e -> `Error (false, e)
      | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
      | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
      | `Ok t ->
          let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
          `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)

let () =
  let open Cmdliner in

  (* Help sections common to all commands *)

  let help_secs = [
  `S Manpage.s_common_options;
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
  `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
  `P "Use `$(mname) help environment' for help on environment variables.";
  `S Manpage.s_bugs; `P "Check bug reports at https://github.com/esy/esy.";]
  in

  (* Options common to all commands *)

  let commonOptions debug verb prefixPath sandboxPath = {
    debug; verb; prefixPath; sandboxPath
  } in

  let copts_t =
    let docs = Manpage.s_common_options in
    let debug =
      let doc = "Give only debug output." in
      Arg.(value & flag & info ["debug"] ~docs ~doc)
    in
    let verb =
      let doc = "Suppress informational output." in
      let quiet = Quiet, Arg.info ["q"; "quiet"] ~docs ~doc in
      let doc = "Give verbose output." in
      let verbose = Verbose, Arg.info ["v"; "verbose"] ~docs ~doc in
      Arg.(last & vflag_all [Normal] [quiet; verbose])
    in
    let prefixPath =
      let doc = "Specifies esy prefix path." in
      let env = Arg.env_var "ESY__PREFIX" ~doc in
      Arg.(value & opt (some string) None & info ["P"; "prefix-path"] ~env ~docv:"PATH" ~doc)
    in
    let sandboxPath =
      let doc = "Specifies esy sandbox path." in
      let env = Arg.env_var "ESY__SANDBOX" ~doc in
      Arg.(value & opt (some string) None & info ["S"; "sandbox-path"] ~env ~docv:"PATH" ~doc)
    in
    Term.(const commonOptions $ debug $ verb $ prefixPath $ sandboxPath)
  in

  (* Command terms *)

  let default_cmd =
    let doc = "esy package builder" in
    let sdocs = Manpage.s_common_options in
    let exits = Term.default_exits in
    let man = help_secs in
    Term.(ret (const build $ copts_t)),
    Term.info "esyb" ~version:"v0.1.0" ~doc ~sdocs ~exits ~man
  in

  let build_cmd =
    let doc = "build package" in
    let sdocs = Manpage.s_common_options in
    let exits = Term.default_exits in
    let man = help_secs in
    Term.(ret (const build $ copts_t)),
    Term.info "build" ~doc ~sdocs ~exits ~man
  in

  let shell_cmd =
    let doc = "shell into build environment" in
    let sdocs = Manpage.s_common_options in
    let exits = Term.default_exits in
    let man = help_secs in
    Term.(ret (const shell $ copts_t)),
    Term.info "shell" ~doc ~sdocs ~exits ~man
  in

  let exec_cmd =
    let doc = "execute command inside build environment" in
    let sdocs = Manpage.s_common_options in
    let exits = Term.default_exits in
    let man = help_secs in
    Term.(ret (const shell $ copts_t)),
    Term.info "exec" ~doc ~sdocs ~exits ~man
  in

  let help_cmd =
    let topic =
      let doc = "The topic to get help on. `topics' lists the topics." in
      Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
    in
    let doc = "display help about esyb and its commands" in
    let man =
      [`S Manpage.s_description;
      `P "Prints help about esyb commands and other subjects...";
      `Blocks help_secs; ]
    in
    Term.(ret (const help $ copts_t $ Arg.man_format $ Term.choice_names $topic)),
    Term.info "help" ~doc ~exits:Term.default_exits ~man
  in

  let cmds = [
    build_cmd;
    shell_cmd;
    exec_cmd;
    help_cmd
  ] in

  Term.(exit @@ eval_choice default_cmd cmds)
