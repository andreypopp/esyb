# esyb

A package builder for [esy][].

## How it works

A single command `esyb` operates on a build description, it allows to:

- Build a package with `esyb build` command.
- Shell into the build environment with `esyb shell`.
- Execute commands with the build environment with `esyb exec -- <command>`.

### Build description

Build description is a JSON files with the information about a package's build
environment and commands needed to build the package and install its artifacts
to the store.

Example:

```json
{
  "id": "pkg-1.0.0-somehash",
  "name": "pkg",
  "version": "1.0.0",
  "sourceType": "immutable",
  "buildType": "_build",
  "build": [
    ["jbuilder", "build"]
  ],
  "install": [
    ["jbuilder", "install"]
  ],
  "sourceDir": "%sandbox%",
  "stageDir": "%localStore%/s/name",
  "installDir": "%localStore%/i/name",
  "buildDir": "%localStore%/b/name",
  "env": {
    "cur__name": "pkg",
    "cur__install": "%localStore%/s/name",
    ...
  }
}
```

Usually you get those build description from esy.

Note that some properties are allowed to use `%name%` variables:

- `%sandbox%` — the absolute path to the sandbox.
- `%store%` — the absolute path to the store.
- `%localStore%` — the absolute path to the sandbox-local store.

This is needed to allow build descriptions not to be tied to a concrete host.

## Development

```
% npm install -g esy
% esy install
% esy build
% esy x esyb
```

[esy]: http://esy.sh
