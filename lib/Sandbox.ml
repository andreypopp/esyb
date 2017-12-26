type pattern =
  | Subpath of Fpath.t
  | Regex of Fpath.t

type config = {
  allowWrite : pattern list;
}

module Darwin = struct

  let renderConfig config =
    let open Sexp in
    let v x = Value (L x) in
    let renderAllowWrite = List.map (function
      | Subpath p ->
        v [I "allow"; I "file-write*"; L [I "subpath"; S (Fpath.to_string p)]];
      | Regex p ->
        v [I "allow"; I "file-write*"; L [I "regex"; S (Fpath.to_string p)]];
    )
    in
    let doc = [
      v [I "version"; N 1.0];
      v [I "allow"; I "default"];
      v [I "deny"; I "file-write*";
         L [I "subpath"; S "/"]];
      v [I "allow"; I "file-write*";
         L [I "literal"; S "/dev/null"]];
    ] @ (renderAllowWrite config.allowWrite)
    in render doc

  let sandboxExec config = Run.(
    let configData = renderConfig config in
    let%bind configFilename = Bos.OS.File.tmp "%s" in
    let%bind () = Bos.OS.File.write configFilename configData in
    let exec command =
      let sandboxCommand = Bos.Cmd.of_list [
        "sandbox-exec"; "-f"; Fpath.to_string configFilename
      ] in
      let command = Bos.Cmd.(sandboxCommand %% command) in
      Bos.OS.Cmd.run command
    in Ok exec
  )
end

module NoSandbox = struct

  let sandboxExec _config =
    let exec command = Bos.OS.Cmd.run command
    in Ok exec
end

let sandboxExec config =
  match Run.uname () with
  | "darwin" -> Darwin.sandboxExec config
  | _ -> NoSandbox.sandboxExec config
