type t = {
  sandboxPath: Fpath.t ;
  storePath: Fpath.t ;
  localStorePath: Fpath.t
} [@@deriving (show)]

let storeInstallTree = "i"
let storeBuildTree = "b"
let storeStageTree = "s"
let storeVersion = "3"

let maxShebangLength = 127
let ocamlrunStorePath = "ocaml-n.00.000-########/bin/ocamlrun"

let storePaddingLength = maxShebangLength
  - String.length "!#"
  - String.length ("/" ^ storeInstallTree ^ "/" ^ ocamlrunStorePath)

let storePadding =
  String.make storePaddingLength '_'

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
    let prefixPathLength = String.length (Fpath.to_string (prefixPath / storeVersion)) in
    let paddingLength = storePaddingLength - prefixPathLength in
    String.make paddingLength '_'
  in
  let storePath = prefixPath / (storeVersion ^ storePadding) in
  let localStorePath = sandboxPath / "node_modules" / ".cache" / "_esy" / "store" in
  Ok {storePath; sandboxPath; localStorePath}
)
