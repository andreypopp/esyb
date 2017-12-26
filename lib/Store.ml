type t = {
  path: Fpath.t
}

let create (path : Fpath.t) = Run.(
  let%bind () = mkdir Fpath.(path / "i") in
  let%bind () = mkdir Fpath.(path / "b") in
  let%bind () = mkdir Fpath.(path / "s") in
  Ok({path})
)
