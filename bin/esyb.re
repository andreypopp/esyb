open EsyLib;

module File = Bos.OS.File;

module Dir = Bos.OS.Dir;

type verb =
  | Normal
  | Quiet
  | Verbose;

type commonOptions = {
  debug: bool,
  verb,
  prefixPath: option(Fpath.t),
  sandboxPath: option(Fpath.t)
};

let build = config => {
  print_endline(Config.show(config));
  open Run;
  let buildPath = v("fixtures") / "simple" / "build.json";
  let%bind spec = BuildSpec.ofFile(buildPath);
  let%bind () = Builder.build(spec);
  Ok();
};

let shell = _copts => Ok();

let exec = _copts => Ok();

let runToCompletion = (command, copts: commonOptions) => {
  Logs.set_reporter(Logs_fmt.reporter());
  let run = {
    open Run;
    let {prefixPath, sandboxPath} = copts;
    let%bind config = Config.create(~prefixPath, ~sandboxPath, ());
    command(config);
  };
  switch run {
  | Error(`Msg(msg)) => `Error((false, msg))
  | _ => `Ok()
  };
};

let help = (_copts, man_format, cmds, topic) =>
  switch topic {
  | None => `Help((`Pager, None)) /* help about the program. */
  | Some(topic) =>
    let topics = ["topics", "patterns", "environment", ...cmds];
    let (conv, _) = Cmdliner.Arg.enum(List.rev_map(s => (s, s), topics));
    switch (conv(topic)) {
    | `Error(e) => `Error((false, e))
    | `Ok(t) when t == "topics" =>
      List.iter(print_endline, topics);
      `Ok();
    | `Ok(t) when List.mem(t, cmds) => `Help((man_format, Some(t)))
    | `Ok(t) =>
      let page = ((topic, 7, "", "", ""), [`S(topic), `P("Say something")]);
      `Ok(Cmdliner.Manpage.print(man_format, Format.std_formatter, page));
    };
  };

let () = {
  open Cmdliner;
  /* Help sections common to all commands */
  let help_secs = [
    `S(Manpage.s_common_options),
    `P("These options are common to all commands."),
    `S("MORE HELP"),
    `P("Use `$(mname) $(i,COMMAND) --help' for help on a single command."),
    `Noblank,
    `P("Use `$(mname) help patterns' for help on patch matching."),
    `Noblank,
    `P("Use `$(mname) help environment' for help on environment variables."),
    `S(Manpage.s_bugs),
    `P("Check bug reports at https://github.com/esy/esy.")
  ];
  /* Options common to all commands */
  let commonOptions = (debug, verb, prefixPath, sandboxPath) => {
    debug,
    verb,
    prefixPath,
    sandboxPath
  };
  let path = {
    let parse = Fpath.of_string;
    let print = Fpath.pp;
    Arg.conv(~docv="PATH", (parse, print));
  };
  let copts_t = {
    let docs = Manpage.s_common_options;
    let debug = {
      let doc = "Give only debug output.";
      Arg.(value & flag & info(["debug"], ~docs, ~doc));
    };
    let verb = {
      let doc = "Suppress informational output.";
      let quiet = (Quiet, Arg.info(["q", "quiet"], ~docs, ~doc));
      let doc = "Give verbose output.";
      let verbose = (Verbose, Arg.info(["v", "verbose"], ~docs, ~doc));
      Arg.(last & vflag_all([Normal], [quiet, verbose]));
    };
    let prefixPath = {
      let doc = "Specifies esy prefix path.";
      let env = Arg.env_var("ESY__PREFIX", ~doc);
      Arg.(
        value
        & opt(some(path), None)
        & info(["P", "prefix-path"], ~env, ~docv="PATH", ~doc)
      );
    };
    let sandboxPath = {
      let doc = "Specifies esy sandbox path.";
      let env = Arg.env_var("ESY__SANDBOX", ~doc);
      Arg.(
        value
        & opt(some(path), None)
        & info(["S", "sandbox-path"], ~env, ~docv="PATH", ~doc)
      );
    };
    Term.(const(commonOptions) $ debug $ verb $ prefixPath $ sandboxPath);
  };
  /* Command terms */
  let default_cmd = {
    let doc = "esy package builder";
    let sdocs = Manpage.s_common_options;
    let exits = Term.default_exits;
    let man = help_secs;
    (
      Term.(ret(const(runToCompletion(build)) $ copts_t)),
      Term.info("esyb", ~version="v0.1.0", ~doc, ~sdocs, ~exits, ~man)
    );
  };
  let build_cmd = {
    let doc = "build package";
    let sdocs = Manpage.s_common_options;
    let exits = Term.default_exits;
    let man = help_secs;
    (
      Term.(ret(const(runToCompletion(build)) $ copts_t)),
      Term.info("build", ~doc, ~sdocs, ~exits, ~man)
    );
  };
  let shell_cmd = {
    let doc = "shell into build environment";
    let sdocs = Manpage.s_common_options;
    let exits = Term.default_exits;
    let man = help_secs;
    (
      Term.(ret(const(runToCompletion(shell)) $ copts_t)),
      Term.info("shell", ~doc, ~sdocs, ~exits, ~man)
    );
  };
  let exec_cmd = {
    let doc = "execute command inside build environment";
    let sdocs = Manpage.s_common_options;
    let exits = Term.default_exits;
    let man = help_secs;
    (
      Term.(ret(const(runToCompletion(exec)) $ copts_t)),
      Term.info("exec", ~doc, ~sdocs, ~exits, ~man)
    );
  };
  let help_cmd = {
    let topic = {
      let doc = "The topic to get help on. `topics' lists the topics.";
      Arg.(value & pos(0, some(string), None) & info([], ~docv="TOPIC", ~doc));
    };
    let doc = "display help about esyb and its commands";
    let man = [
      `S(Manpage.s_description),
      `P("Prints help about esyb commands and other subjects..."),
      `Blocks(help_secs)
    ];
    (
      Term.(
        ret(const(help) $ copts_t $ Arg.man_format $ Term.choice_names $ topic)
      ),
      Term.info("help", ~doc, ~exits=Term.default_exits, ~man)
    );
  };
  let cmds = [build_cmd, shell_cmd, exec_cmd, help_cmd];
  Term.(exit @@ eval_choice(default_cmd, cmds));
};
