type t = {
  sandboxPath: Fpath.t ;
  storePath: Fpath.t ;
  localStorePath: Fpath.t
} [@@deriving (show)]

let storeInstallTree = "i"
let storeBuildTree = "b"
let storeStageTree = "s"
let storeVersion = "3"

let maxStorePaddingLength =
  (* This is restricted by POSIX, Linux enforces this but macOS is more
   * forgiving. *)
  let maxShebangLength = 127 in
  (* We reserve that amount of chars from padding so ocamlrun can be placed i
   * shebang lines *)
  let ocamlrunStorePath = "ocaml-n.00.000-########/bin/ocamlrun" in
  maxShebangLength
  - String.length "!#"
  - String.length ("/" ^ storeVersion ^ "/" ^ storeInstallTree ^ "/" ^ ocamlrunStorePath)

let create ~prefixPath ~sandboxPath () = Run.(
  let%bind prefixPath = match prefixPath with
  | Some v -> Ok v
  | None ->
    let%bind home = Bos.OS.Dir.user () in
    Ok (home / ".esy")
  in
  let%bind sandboxPath = match sandboxPath with
  | Some v -> Ok v
  | None ->
    Bos.OS.Dir.current ()
  in
  let storePadding =
    let prefixPathLength = String.length (Fpath.to_string prefixPath) in
    let paddingLength = maxStorePaddingLength - prefixPathLength in
    String.make paddingLength '_'
  in
  let storePath = prefixPath / (storeVersion ^ storePadding) in
  let localStorePath = sandboxPath / "node_modules" / ".cache" / "_esy" / "store" in
  Ok {storePath; sandboxPath; localStorePath}
)
