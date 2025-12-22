Write in F#, with the intention to translate to Darklang later. Thus:

- Use a limited set of F#
- use only functional constructs and no imperative ones
- don't use dotnet libraries much
- obvious exception: when doing system calls, filesystem updates, etc

Compiler structure:

- Compiler passes should start with a prefix of their numbering in the pass order in the compiler pipeline.
- There should only be one compiler pass per file.

Comments:

- Every file should have a comment at the top explaining what the file is for.

- Comments should be generated for an experienced senior compiler engineer / developer

Misc:

- Keep README.md and docs/ files updated
