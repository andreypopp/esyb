let parseWith parser data =
  let json = Yojson.Safe.from_string data in
  match parser json with
  | Ok value -> Ok value
  | Error msg -> Error (`Msg msg)
