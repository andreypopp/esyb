open EsyLib

module File = Bos.OS.File
module Dir = Bos.OS.Dir

let parseWith parser data =
  let json = Yojson.Safe.from_string data in
  match parser json with
  | Ok value -> Ok value
  | Error msg -> Error (`Msg msg)

let ofFile (path : Fpath.t) = Run.(
  let%bind data = File.read path in
  parseWith BuildSpec.of_yojson data
)

let exitOnError r =
  match r with
  | Error (`Msg msg) ->
    exit 1
  | _ -> ()

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  exitOnError Run.(
    let buildPath = v "fixtures" / "simple" / "build.json" in
    let%bind spec = ofFile buildPath in
    let%bind () = Builder.build spec in
    Ok ()
  )
