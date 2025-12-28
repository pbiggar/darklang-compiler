Write in F#, with the intention to translate to Darklang later. Thus:

- Use a limited set of F#
- use only functional constructs and no imperative ones
- use `Option`s and `Result`s a lot - dont use success flags or other imperative patterns
- Don't use exceptions or `failwith` (or similar) or `exit`
- Don't use `find` methods which throw exceptions
- don't use dotnet libraries much
- completely avoid all mutable values
- use string interpolation instead of printf-style calls, don't use `printf`, `printfn`, `eprintf`, of `eprintfn`
- if a function or method we need to use throws an exception, create a wrapper function which uses `Result` instead.

Dotnet:

- The dotnet compiler often hangs, do not use a long timeout

Compiler structure:

- Compiler passes should start with a prefix of their numbering in the pass order in the compiler pipeline.
- There should only be one compiler pass per file.

Comments:

- Every file should have a comment at the top explaining what the file is for.

- Comments should be generated for an experienced senior compiler engineer / developer

Tests:

- Write tests first if possible
- For any bug or issue being fixed, first identify and add a test which exposes the bug, and use that to validate the fix works.
- if a bug is found, add the smallest test for that bug in the right place.
- Otherwise, largely focus on end-to-end language tests
- if at decision points, it seems the tests are failing, fix the tests if possible
- never disable failing tests, or change test cases to no longer hit the bug

Running tests:

- Use `./run-tests` to build and run all tests
- Use `./run-tests --filter=PATTERN` to run only tests matching PATTERN (case-insensitive)
- Use `./run-tests --parallel=N` to run with N parallel workers
- Use `./run-tests --help` for full usage information
- Common filter patterns: tuple, record, list, string, float, closure, match, adt, generic, stdlib

Best-practices:

- Keep README.md, TODO.md, and docs/ files updated
- Always fix dotnet compiler or runtime warnings and errors, preferably before commiting (make a separate cleanup commit if needed)

  - For dumb warnings, ask the developer if you want to disable
  - when fixing tests, ensure that its the compiler that's fixed, and that the tests are not changed to avoid the error. It's ok to add and commit tests that work incorrectly, in order to fix later.

- Never ever ever make assumptions, fall-backs, or potentially incorrect defaults. Add a call to TODO() instead.
