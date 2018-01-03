let isExecutable = (stats: Unix.stats) => {
  let userExecute = 0b001000000;
  let groupExecute = 0b000001000;
  let othersExecute = 0b000000001;
  userExecute lor groupExecute lor othersExecute land stats.Unix.st_perm != 0;
};

let resolveCmd = (path, cmd) => {
  open Run;
  let find = p => {
    let p = v(p) / cmd;
    let%bind stats = Bos.OS.Path.stat(p);
    switch (stats.Unix.st_kind, isExecutable(stats)) {
    | (Unix.S_REG, true) => Ok(Some(p))
    | _ => Ok(None)
    };
  };
  let rec _resolveCmd =
    fun
    | [] => Error(`Msg("unable to resolve command: " ++ cmd))
    | ["", ...xs] => _resolveCmd(xs)
    | [x, ...xs] =>
      switch (find(x)) {
      | Ok(Some(x)) => Ok(Path.to_string(x))
      | Ok(None)
      | Error(_) => _resolveCmd(xs)
      };
  _resolveCmd(path);
};

let resolveInvocation = (path, cmd) => {
  open Run;
  let cmd = Bos.Cmd.to_list(cmd);
  switch cmd {
  | [] => Error(`Msg("empty command"))
  | [cmd, ...args] =>
    let%bind cmd = resolveCmd(path, cmd);
    Ok(Bos.Cmd.of_list([cmd, ...args]));
  };
};
