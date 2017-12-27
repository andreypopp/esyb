/**
 * This module implements utilities which are used to "script" build processes.
 */
type t('a) = result(unit, [> Rresult.R.msg] as 'a);

let ok = Result.ok;

let (/) = Fpath.(/);

let v = Fpath.v;

let withCwd = (path, ~f) => Result.join(Bos.OS.Dir.with_current(path, f, ()));

let mkdir = path =>
  switch (Bos.OS.Dir.create(path)) {
  | Ok(_) => Ok()
  | Error(msg) => Error(msg)
  };

let rmdir = path => Bos.OS.Dir.delete(~recurse=true, path);

let rm = path => Bos.OS.File.delete(path);

let uname = () => {
  let ic = Unix.open_process_in("uname");
  let uname = input_line(ic);
  let () = close_in(ic);
  String.lowercase_ascii(uname);
};

module Let_syntax = Result.Let_syntax;

/**
 * Put temporary file into filesystem with the specified contents and return its
 * filename. This temporary file will be cleaned up at exit.
 */
let putTempFile = (contents: string) => {
  let%bind filename = Bos.OS.File.tmp("%s");
  let%bind () = Bos.OS.File.write(filename, contents);
  Ok(filename);
};

let traverse = (path: Fpath.t, f: (Fpath.t, Unix.stats) => t(_)) : t(_) => {
  let visit = (path: Fpath.t) =>
    fun
    | Ok () => {
        let%bind stats = Bos.OS.Path.symlink_stat(path);
        f(path, stats);
      }
    | error => error;
  Result.join(Bos.OS.Path.fold(~dotfiles=true, visit, Ok(), [path]));
};
