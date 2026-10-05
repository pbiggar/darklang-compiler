(*
   RCReleaseFormat.fs - Parser for semantic reference-release fixtures.
   Describes canonical managed object graphs without exposing their LIR layout.
*)
open Dark_compiler
type managedShape=Int64Value|EnumValue|DynamicString|LiteralString|DynamicBlob|ListValue of managedShape|DictValue of managedShape*managedShape|TupleValue of managedShape list|RecordValue of managedShape list|SumValue of managedShape|ClosureValue of managedShape list
type preservedRegister={register:LIR.physReg;value:int64}
type rootPlacement=CanonicalRoot|ExplicitRoot of LIR.physReg*preservedRegister list
type rCReleaseTest={name:string;root:managedShape;placement:rootPlacement;sourceFile:string}
val parseRCReleaseFileContent : string -> string -> (rCReleaseTest list,string) result
