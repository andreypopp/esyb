type sourceType =
  | Immutable
  | Transient
  | Root [@@deriving show]

let sourceType_of_yojson (json : Yojson.Safe.json) =
  match json with
  | `String "immutable" -> Ok Immutable
  | `String "transient" -> Ok Transient
  | `String "root" -> Ok Root
  | _ -> Error "invalid buildType"

type buildType =
  | InSource
  | JbuilderLike
  | OutOfSource [@@deriving show]

let buildType_of_yojson (json : Yojson.Safe.json) =
  match json with
  | `String "in-source" -> Ok InSource
  | `String "out-of-source" -> Ok OutOfSource
  | `String "_build" -> Ok JbuilderLike
  | _ -> Error "invalid buildType"

module Cmd = struct
  module JsonRepr = struct
    type t = string list [@@deriving of_yojson]
  end

  type t = Bos.Cmd.t
  let pp = Bos.Cmd.pp
  let of_yojson (json : Yojson.Safe.json) = Result.(
    let%bind items = JsonRepr.of_yojson json in
    Ok (Bos.Cmd.of_list items)
  )
end

module Fpath = struct
  include Fpath
  let of_yojson (json : Yojson.Safe.json) =
    match json with
    | `String v ->
        (match Fpath.of_string v with
          | Ok v -> Ok v
          | Error (`Msg msg) -> Error msg)
    | _ -> Error "invalid path"
end

module Env = struct
  type t = Bos.OS.Env.t

  let pp _fmt _env = ()

  let of_yojson (json : Yojson.Safe.json) =
    match json with
    | `Assoc items ->
      let add_to_map res (key, value) =
        match res, value with
        | Ok res, `String value ->
          Ok (Astring.String.Map.add key value res)
        | _ ->
          Error "expected a string value"
      in
      List.fold_left add_to_map (Ok Astring.String.Map.empty) items
    | _ -> Error "expected object"

end

type t = {
  id: string;
  name: string;
  version: string;
  sourceType: sourceType;
  buildType: buildType;
  build: Cmd.t list;
  install: Cmd.t list;
  sourceDir: Fpath.t;
  stageDir: Fpath.t;
  installDir: Fpath.t;
  buildDir: Fpath.t;
  env : Env.t;
} [@@deriving (show, of_yojson)]

let ofFile (path : Fpath.t) = Run.(
  let%bind data = Bos.OS.File.read path in
  let%bind spec = Json.parseWith of_yojson data in
  Ok spec
)
