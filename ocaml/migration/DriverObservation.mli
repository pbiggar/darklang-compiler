(* Complete public driver boundary observations without opening checked catalogs. *)
open Dark_compiler
val symbols : CheckedAST.symbols -> Yojson.Basic.t
val program : CheckedAST.program -> Yojson.Basic.t
val registries : AST_to_ANF.registries -> Yojson.Basic.t
val declaration : SourcePreparation.declarationConversion -> Yojson.Basic.t
val conversion : AST_to_ANF.conversionResult -> Yojson.Basic.t
val user : AST_to_ANF.userOnlyResult -> Yojson.Basic.t
