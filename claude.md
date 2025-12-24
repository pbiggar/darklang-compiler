Write in F#, with the intention to translate to Darklang later. Thus:

- Use a limited set of F#
- use only functional constructs and no imperative ones
- use Options and Results a lot - dont use success flags or other imperative patterns
- Don't use exceptions or failwith (or similar)
- don't use dotnet libraries much
- use string interpolation instead of printf-style calls
- obvious exception: when doing system calls, filesystem updates, etc

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
- if a bug is found, add the smallest test for that bug in the right place.
- Otherwise, largely focus on end-to-end language tests

Best-practices:

- Keep README.md, TODO.md, and docs/ files updated
- Always fix dotnet compiler or runtime warnings and errors, preferably before commiting (make a separate cleanup commit if needed)
- For dumb warnings, ask the developer if you want to disable
