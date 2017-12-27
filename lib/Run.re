module Let_syntax = Result.Let_syntax;

let ok = Result.ok;

let (/) = Fpath.(/);

let v = Fpath.v;

let withCwd = (path, ~f) => Result.join(Bos.OS.Dir.with_current(path, f, ()));

let mkdir = path => {
  let%bind _ = Bos.OS.Dir.create(path);
  ok;
};

let rmdir = path => Bos.OS.Dir.delete(~recurse=true, path);

let rm = path => Bos.OS.File.delete(path);

let uname = () => {
  let ic = Unix.open_process_in("uname");
  let uname = input_line(ic);
  let () = close_in(ic);
  String.lowercase_ascii(uname);
};
