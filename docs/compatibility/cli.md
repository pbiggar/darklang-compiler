# CLI compatibility

Current imported declarations and tests are tracked in the
[source ledger](../porting/README.md). The following documents own the public
behavior and implementation boundaries:

- [Presentation](cli-presentation.md): exact output, prompts, key input and UI.
- [Processes, host discovery and input](cli-process-host-input.md).
- [POSIX filesystem and descriptor APIs](posix-host-api.md).
- [Deferred package-review decisions](package-review.md).

`Stdlib.Cli.Path` supplies basename, parent, extension, normalization, joining,
resolution and relative paths. Pure glob segment matching handles literals,
`*`, `?` and recursive `**`; multiple stars within one segment must preserve
matching order (`a*b*c` matches `axbyc`, not `axbyd`). Focused coverage is in
`cli_filesystem.e2e` and the imported path/glob tests.

The public filesystem surface is `Stdlib.Cli.FileSystem`. Its native operation
boundaries are private compiler intrinsics registered in `src/DarkStdlib.ml`.
`library-sources.list` owns the embedded module layout. Relative path resolution
may consult the native current directory.

The compiler may diagnose statically knowable errors before execution. That
phase difference does not authorize different successful values, errno records,
side effects or resource lifecycles. An omitted API is not evidence of either
parity or an intentional exclusion; consult its source annotation.
