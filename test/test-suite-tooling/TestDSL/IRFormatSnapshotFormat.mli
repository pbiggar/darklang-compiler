(* Typed inputs for exact IR formatter snapshots. *)
type irFormatInput=ANFInput of Dark_compiler.ANF.program | MIRInput of Dark_compiler.MIR.program | LIRInput of Dark_compiler.LIR.program
type irFormatSnapshotTest={name:string;input:irFormatInput;expected:string;sourceFile:string}
val parseIRFormatSnapshotFileContent : string -> string -> (irFormatSnapshotTest list,string) result
