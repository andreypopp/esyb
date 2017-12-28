type pattern =
  | Subpath(string)
  | Regex(string);

type config = {allowWrite: list(pattern)};

module type Sandbox = {
  let sandboxExec:
    config =>
    result(
      (~env: Bos.OS.Env.t, Bos.Cmd.t) => result(unit, [> Result.R.msg]),
      [> Rresult.R.msg] as 'a
    );
};

module Darwin: Sandbox = {
  let renderConfig = config => {
    open Sexp;
    let v = x => Value(L(x));
    let renderAllowWrite =
      List.map(
        fun
        | Subpath(p) =>
          v([I("allow"), I("file-write*"), L([I("subpath"), S(p)])])
        | Regex(p) => v([I("allow"), I("file-write*"), L([I("regex"), S(p)])])
      );
    let doc =
      [
        v([I("version"), N(1.0)]),
        v([I("allow"), I("default")]),
        v([I("deny"), I("file-write*"), L([I("subpath"), S("/")])]),
        v([I("allow"), I("file-write*"), L([I("literal"), S("/dev/null")])])
      ]
      @ renderAllowWrite(config.allowWrite);
    render(doc);
  };
  let sandboxExec = config => {
    open Run;
    let configData = renderConfig(config);
    let%bind configFilename = putTempFile(configData);
    let exec = (~env, command) => {
      let sandboxCommand =
        Bos.Cmd.of_list([
          "sandbox-exec",
          "-f",
          Fpath.to_string(configFilename)
        ]);
      let command = Bos.Cmd.(sandboxCommand %% command);
      Bos.OS.Cmd.run(~env, command);
    };
    Ok(exec);
  };
};

module NoSandbox: Sandbox = {
  let sandboxExec = _config => {
    let exec = (~env, command) => Bos.OS.Cmd.run(~env, command);
    Ok(exec);
  };
};

let sandboxExec = config =>
  switch (Run.uname()) {
  | "darwin" => Darwin.sandboxExec(config)
  | _ => NoSandbox.sandboxExec(config)
  };
