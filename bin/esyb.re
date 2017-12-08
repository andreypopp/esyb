open EsyLib;

module File = Bos.OS.File;

module Dir = Bos.OS.Dir;

let parseWith = (parser, data) => {
  let json = Yojson.Safe.from_string(data);
  switch (parser(json)) {
  | Ok(value) => Ok(value)
  | Error(msg) => Error(`Msg(msg))
  };
};

let ofFile = (path: Fpath.t) =>
  Result.(
    {
      let data = do File.read(path);
      print_endline(data);
      parseWith(BuildSpec.of_yojson, data);
    }
  );

let exitOnError = (r) =>
  switch r {
  | Error(`Msg(msg)) =>
    print_endline(msg);
    exit(1);
  | _ => ()
  };

let main = () => {
  Logs.set_reporter(Logs_fmt.reporter());
  exitOnError(
    {
      open Result;
      let buildPath = Fpath.(v("fixtures") / "simple" / "build.json");
      let spec = do ofFile(buildPath);
      do Builder.build(spec);
      Ok();
    }
  );
};

main();
