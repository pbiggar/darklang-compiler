(*
   X86_64EncodingFormat.mli - Parser for multi-case x64 encoding and resolution fixtures.
   A successful case can assert final bytes, deferred fixup labels, or both.
*)
type x64EncodingExpectation=ResolvesTo of bytes option * string list | ResolutionErrorContaining of string
type x64EncodingTest={name:string;instructions:Dark_compiler.X86_64.instr list;expectation:x64EncodingExpectation;sourceFile:string}
val parseX64EncodingFileContent : string -> string -> (x64EncodingTest list,string) result
