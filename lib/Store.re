type t = {path: Fpath.t};

let create = (path: Fpath.t) =>
  Run.(
    {
      let%bind () = mkdir(Fpath.(path / "i"));
      let%bind () = mkdir(Fpath.(path / "b"));
      let%bind () = mkdir(Fpath.(path / "s"));
      Ok({path: path});
    }
  );
