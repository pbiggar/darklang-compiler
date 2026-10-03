(* Full constructor and scalar optimizer observations, built from production sources. *)
[@@@warning "-4"]
open Dark_compiler
module A = InstrumentedANF
module C = InstrumentedANFConstants
module S = InstrumentedANFSubstitution
module E = InstrumentedANFEffects
module I = InstrumentedInliningCommon
module D = InstrumentedANFDeadCodeElimination
module J = SemanticANF
open A
open C
module FS = InstrumentedSpecializationIdentity.FunctionSet
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let int n = `Assoc ["kind", `String "int32"; "value", `String (string_of_int n)]
let int64 n = `Assoc ["kind", `String "int64"; "value", `String (Int64.to_string n)]
let option f = function None -> SemanticJson.union "FSharpOption" "None" [] | Some x -> SemanticJson.union "FSharpOption" "Some" [f x]
let functionId id = SemanticJson.union "FunctionId" "FunctionId" [`Assoc ["kind",`String "uint64";"value",`String (Z.to_string (if AST.functionIdValue id < 0L then Z.add (Z.of_int64 (AST.functionIdValue id)) (Z.shift_left Z.one 64) else Z.of_int64 (AST.functionIdValue id)))]]
let observe source =
 let fixtures = [(InstrumentedANF.Atom ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.TypedAtom ((InstrumentedANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
(InstrumentedANF.Prim ((InstrumentedANF.Add), (InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source)));
(InstrumentedANF.UnaryPrim ((InstrumentedANF.Neg), (InstrumentedANF.StringLiteral source)));
(InstrumentedANF.IfValue ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source)));
(InstrumentedANF.Call ((Dark_compiler.AST.functionId (-1L)), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.BorrowedCall ((Dark_compiler.AST.functionId (-1L)), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.TailCall ((Dark_compiler.AST.functionId (-1L)), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.IndirectCall ((InstrumentedANF.StringLiteral source), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.IndirectTailCall ((InstrumentedANF.StringLiteral source), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.ClosureAlloc ((Dark_compiler.AST.functionId (-1L)), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.ClosureCall ((InstrumentedANF.StringLiteral source), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.ClosureTailCall ((InstrumentedANF.StringLiteral source), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.TupleAlloc ([(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.TupleGet ((InstrumentedANF.StringLiteral source), (3)));
(InstrumentedANF.RecordAlloc (({InstrumentedANF.sourceTypeName = (source); InstrumentedANF.runtimeTypeName = (source); InstrumentedANF.typeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; InstrumentedANF.fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; InstrumentedANF.valueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : InstrumentedANF.recordDescriptor), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.RecordGet (({InstrumentedANF.sourceTypeName = (source); InstrumentedANF.runtimeTypeName = (source); InstrumentedANF.typeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; InstrumentedANF.fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; InstrumentedANF.valueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : InstrumentedANF.recordDescriptor), (InstrumentedANF.StringLiteral source), (3)));
(InstrumentedANF.RecordClone (({InstrumentedANF.sourceTypeName = (source); InstrumentedANF.runtimeTypeName = (source); InstrumentedANF.typeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; InstrumentedANF.fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; InstrumentedANF.valueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : InstrumentedANF.recordDescriptor), (InstrumentedANF.StringLiteral source), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.RecordReuse (({InstrumentedANF.sourceTypeName = (source); InstrumentedANF.runtimeTypeName = (source); InstrumentedANF.typeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; InstrumentedANF.fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; InstrumentedANF.valueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : InstrumentedANF.recordDescriptor), ({InstrumentedANF.sourceTypeName = (source); InstrumentedANF.runtimeTypeName = (source); InstrumentedANF.typeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; InstrumentedANF.fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; InstrumentedANF.valueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : InstrumentedANF.recordDescriptor), (InstrumentedANF.StringLiteral source), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.StringConcat ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.CanonicalBufferEq ((Dark_compiler.MemoryModel.Utf8String), (InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RefCountInc ((InstrumentedANF.StringLiteral source), (3), (Dark_compiler.MemoryModel.GenericHeap), (Some (({Dark_compiler.MemoryModel.releasePlanCacheKey = (Some ((source))); Dark_compiler.MemoryModel.releasePlan = (Some ((Dark_compiler.MemoryModel.NoReleasePlan))); Dark_compiler.MemoryModel.sourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : Dark_compiler.MemoryModel.rcMetadata)))));
(InstrumentedANF.RefCountDec ((InstrumentedANF.StringLiteral source), (3), (Dark_compiler.MemoryModel.GenericHeap), (Some (({Dark_compiler.MemoryModel.releasePlanCacheKey = (Some ((source))); Dark_compiler.MemoryModel.releasePlan = (Some ((Dark_compiler.MemoryModel.NoReleasePlan))); Dark_compiler.MemoryModel.sourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : Dark_compiler.MemoryModel.rcMetadata)))));
(InstrumentedANF.Print ((InstrumentedANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
(InstrumentedANF.StdoutWrite ((InstrumentedANF.StringLiteral source), (true)));
(InstrumentedANF.StdinReadLine);
(InstrumentedANF.RuntimeError ((source)));
(InstrumentedANF.RuntimeErrorString ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FileReadBlob ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FileExists ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FileWriteBlob ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FileAppendText ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FileDelete ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FileCreateDirectory ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FileSetExecutable ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FileWriteFromPtr ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FloatSqrt ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FloatAbs ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FloatNeg ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.Int64ToFloat ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FloatToInt64 ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FloatToBits ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawAlloc ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.MappedAlloc ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawFree ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.MappedFree ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawGet ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))));
(InstrumentedANF.RawTake ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))));
(InstrumentedANF.RawGetByte ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawWriteWord ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawWriteByte ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawSlotInit ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
(InstrumentedANF.StringToRawPtr ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawPtrToString ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.BlobToRawPtr ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawPtrToBlob ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawPtrToInt128 ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawPtrToUInt128 ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.DictToRawPtr ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawPtrToDict ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
(InstrumentedANF.ListToRawPtr ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.FixedBlockToRawPtr ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RawPtrToList ((InstrumentedANF.StringLiteral source), (InstrumentedANF.StringLiteral source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
(InstrumentedANF.RefCountIncString ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RefCountDecString ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RefCountIncBlob ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RefCountDecBlob ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RefCountIncInt ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RefCountDecInt ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.RandomInt64);
(InstrumentedANF.DateTimeNow);
(InstrumentedANF.Sleep ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.CliNative ((InstrumentedANF.Execute), [(InstrumentedANF.StringLiteral source); (InstrumentedANF.StringLiteral source)]));
(InstrumentedANF.FloatToString ((InstrumentedANF.StringLiteral source)));
(InstrumentedANF.Atom ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.TypedAtom ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
(InstrumentedANF.Prim ((InstrumentedANF.Add), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.UnaryPrim ((InstrumentedANF.Neg), (InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.IfValue ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.Call ((Dark_compiler.AST.functionId (-1L)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.BorrowedCall ((Dark_compiler.AST.functionId (-1L)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.TailCall ((Dark_compiler.AST.functionId (-1L)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.IndirectCall ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.IndirectTailCall ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.ClosureAlloc ((Dark_compiler.AST.functionId (-1L)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.ClosureCall ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.ClosureTailCall ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.TupleAlloc ([(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.TupleGet ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (3)));
(InstrumentedANF.RecordAlloc (({InstrumentedANF.sourceTypeName = (source); InstrumentedANF.runtimeTypeName = (source); InstrumentedANF.typeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; InstrumentedANF.fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; InstrumentedANF.valueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : InstrumentedANF.recordDescriptor), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.RecordGet (({InstrumentedANF.sourceTypeName = (source); InstrumentedANF.runtimeTypeName = (source); InstrumentedANF.typeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; InstrumentedANF.fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; InstrumentedANF.valueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : InstrumentedANF.recordDescriptor), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (3)));
(InstrumentedANF.RecordClone (({InstrumentedANF.sourceTypeName = (source); InstrumentedANF.runtimeTypeName = (source); InstrumentedANF.typeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; InstrumentedANF.fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; InstrumentedANF.valueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : InstrumentedANF.recordDescriptor), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.RecordReuse (({InstrumentedANF.sourceTypeName = (source); InstrumentedANF.runtimeTypeName = (source); InstrumentedANF.typeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; InstrumentedANF.fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; InstrumentedANF.valueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : InstrumentedANF.recordDescriptor), ({InstrumentedANF.sourceTypeName = (source); InstrumentedANF.runtimeTypeName = (source); InstrumentedANF.typeArgs = [(AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])); (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))]; InstrumentedANF.fields = [((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))); ((source), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString])))]; InstrumentedANF.valueType = (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))} : InstrumentedANF.recordDescriptor), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.StringConcat ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.CanonicalBufferEq ((Dark_compiler.MemoryModel.Utf8String), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RefCountInc ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (3), (Dark_compiler.MemoryModel.GenericHeap), (Some (({Dark_compiler.MemoryModel.releasePlanCacheKey = (Some ((source))); Dark_compiler.MemoryModel.releasePlan = (Some ((Dark_compiler.MemoryModel.NoReleasePlan))); Dark_compiler.MemoryModel.sourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : Dark_compiler.MemoryModel.rcMetadata)))));
(InstrumentedANF.RefCountDec ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (3), (Dark_compiler.MemoryModel.GenericHeap), (Some (({Dark_compiler.MemoryModel.releasePlanCacheKey = (Some ((source))); Dark_compiler.MemoryModel.releasePlan = (Some ((Dark_compiler.MemoryModel.NoReleasePlan))); Dark_compiler.MemoryModel.sourceType = (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))} : Dark_compiler.MemoryModel.rcMetadata)))));
(InstrumentedANF.Print ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
(InstrumentedANF.StdoutWrite ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (true)));
(InstrumentedANF.StdinReadLine);
(InstrumentedANF.RuntimeError ((source)));
(InstrumentedANF.RuntimeErrorString ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FileReadBlob ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FileExists ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FileWriteBlob ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FileAppendText ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FileDelete ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FileCreateDirectory ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FileSetExecutable ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FileWriteFromPtr ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FloatSqrt ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FloatAbs ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FloatNeg ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.Int64ToFloat ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FloatToInt64 ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FloatToBits ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawAlloc ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.MappedAlloc ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawFree ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.MappedFree ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawGet ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))));
(InstrumentedANF.RawTake ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (Some ((AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))))));
(InstrumentedANF.RawGetByte ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawWriteWord ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawWriteByte ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawSlotInit ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
(InstrumentedANF.StringToRawPtr ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawPtrToString ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.BlobToRawPtr ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawPtrToBlob ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawPtrToInt128 ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawPtrToUInt128 ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.DictToRawPtr ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawPtrToDict ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
(InstrumentedANF.ListToRawPtr ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.FixedBlockToRawPtr ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RawPtrToList ((InstrumentedANF.Var (InstrumentedANF.TempId 3)), (InstrumentedANF.Var (InstrumentedANF.TempId 3)), (AST.TRecord (source, [AST.TInt64; AST.TList AST.TString]))));
(InstrumentedANF.RefCountIncString ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RefCountDecString ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RefCountIncBlob ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RefCountDecBlob ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RefCountIncInt ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RefCountDecInt ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.RandomInt64);
(InstrumentedANF.DateTimeNow);
(InstrumentedANF.Sleep ((InstrumentedANF.Var (InstrumentedANF.TempId 3))));
(InstrumentedANF.CliNative ((InstrumentedANF.Execute), [(InstrumentedANF.Var (InstrumentedANF.TempId 3)); (InstrumentedANF.Var (InstrumentedANF.TempId 3))]));
(InstrumentedANF.FloatToString ((InstrumentedANF.Var (InstrumentedANF.TempId 3))))] in
 let ints = [Int64.min_int; Int64.max_int; -2L; -1L; 0L; 1L; 2L; 3L; 4L; 63L; 64L; 65L] in
 let floats = [neg_infinity; -3.75; -2.; -1.; -0.; 0.; 0.5; 1.; 2.; 3.75; infinity; Int64.float_of_bits 0x7ff8000000001234L] in
 let atoms = [A.UnitLiteral; A.IntLiteral (A.Int8 (-128)); A.IntLiteral (A.Int16 (-32768)); A.IntLiteral (A.Int32 Int32.min_int); A.IntLiteral (A.UInt8 255); A.IntLiteral (A.UInt16 65535); A.IntLiteral (A.UInt32 4294967295L); A.BoolLiteral true; A.BoolLiteral false; A.StringLiteral source; A.StringLiteral ""; A.StringLiteral "e"; A.StringLiteral "\204\129"; A.Var (A.TempId 3); A.Var (A.TempId 4); A.FuncRef (AST.functionId (-1L))] @ List.concat_map (fun n -> [A.IntLiteral (A.Int64 n); A.IntLiteral (A.UInt64 n)]) ints @ List.map (fun f -> A.FloatLiteral f) floats in
 let ops = [A.Add; A.Sub; A.Mul; A.Div; A.Mod; A.Shl; A.Shr; A.BitAnd; A.BitOr; A.BitXor; A.Eq; A.Neq; A.Lt; A.Gt; A.Lte; A.Gte; A.And; A.Or] in
 let types = [AST.TUnit; AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TList AST.TInt64; AST.TTuple [AST.TString; AST.TInt64]] in
 let typeEnvs = C.TempMap.empty :: List.map (fun typ -> C.TempMap.singleton (A.TempId 3) typ) types in
 let envs = [C.TempMap.empty; C.TempMap.singleton (A.TempId 3) (A.Var (A.TempId 3)); C.TempMap.of_list [A.TempId 3, A.IntLiteral (A.Int64 7L); A.TempId 4, A.StringLiteral source]] in
 let context : C.optimizeContext = {typeReg=StringOrder.Map.singleton source ["value",AST.TVar "a";"nested",AST.TVar "b"]; recordTypeParams=StringOrder.Map.singleton source ["a";"b"]; sumShapeReg=StringOrder.Map.empty; functionNames=FunctionIdMap.ofList [AST.functionId 1L,"Darklang.Stdlib.String.__appendNormalized"; AST.functionId 2L,"Darklang.Stdlib.String.__normalizeAfterConcat"]; functionIds=StringOrder.Map.empty} in
 let options = [C.defaultOptimizeOptions; {C.defaultOptimizeOptions with enableConstFolding=false}; {C.defaultOptimizeOptions with enableStrengthReduction=false}; {C.defaultOptimizeOptions with enableConstFolding=false;enableStrengthReduction=false}] in
 let extra = [A.Call (AST.functionId 1L, [A.StringLiteral "e"; A.StringLiteral "\204\129"]); A.Call (AST.functionId 2L,[A.StringLiteral "e\204\129"]); A.Call (AST.functionId 1L,[A.Var (A.TempId 3); A.StringLiteral ""]); A.Call (AST.functionId 1L,[A.StringLiteral ""; A.Var (A.TempId 3)]); A.TupleGet (A.Var (A.TempId 3),1); A.TupleGet (A.Var (A.TempId 3),2); A.StringConcat (A.StringLiteral "",A.Var (A.TempId 4),[A.StringLiteral ""])] in
 let all = fixtures @ extra @ List.map (fun typ -> A.TypedAtom (A.Var (A.TempId 3),typ)) types in
 let tupleEnv = C.TempMap.singleton (A.TempId 3) (C.IntMap.of_list [0,A.IntLiteral (A.Int64 7L);1,A.StringLiteral source]) in
 let bodies = [A.Return (A.Var (A.TempId 3)); A.Let (A.TempId 3,A.Atom (A.Var (A.TempId 3)), A.Let (A.TempId 4,A.Prim (A.Add,A.Var (A.TempId 3),A.Var (A.TempId 4)),A.Return (A.Var (A.TempId 4)))); A.Join ({A.id=A.TempId 3;typ=AST.TInt64},A.Let (A.TempId 4,A.Atom (A.Var (A.TempId 3)),A.Return (A.Var (A.TempId 4))), A.If (A.Var (A.TempId 3),A.Jump (A.TempId 3,A.Var (A.TempId 4)),A.Let (A.TempId 4,A.Atom (A.Var (A.TempId 3)),A.Jump (A.TempId 3,A.Var (A.TempId 4)))))] in
 let function_ id name body : A.functionDef = {id=AST.functionId id;name;typedParams=[{A.id=A.TempId 3;typ=AST.TInt64}];returnType=AST.TInt64;returnOwnership=A.OwnedReturn;body} in
 let functions = [function_ 0L source (List.fold_right (fun c body -> A.Let (A.TempId 4,c,body)) fixtures (A.Return (A.Var (A.TempId 4)))); function_ 1L "one" (A.Let (A.TempId 4,A.Call (AST.functionId 2L,[]),A.Return (A.Var (A.TempId 4)))); function_ 2L "two" (A.Let (A.TempId 4,A.Call (AST.functionId 1L,[]),A.Return (A.Var (A.TempId 4)))); function_ 3L "self" (A.Let (A.TempId 4,A.Call (AST.functionId 3L,[]),A.Return (A.Var (A.TempId 4)))); function_ (-1L) "Darklang.Stdlib.Json.__test" (A.Return A.UnitLiteral)] in
 let graph = FunctionIdMap.map (fun _ (info : I.functionInfo) -> info.I.calls) (I.buildFunctionInfoMap functions) in
 let ids = FS.of_list (List.map (fun (f : A.functionDef) -> f.id) functions) in
 let graphJson graph = list (fun (id,calls) -> tuple [functionId id;list functionId (FS.elements calls)]) (FunctionIdMap.toList graph) in
 let infoJson info = tuple [J.aNF_functionDef info.I.func;list functionId (FS.elements info.I.calls);int info.I.size;`Bool info.I.isRecursive;`Bool info.I.hasClosures;`Bool info.I.hasTailCalls;`Bool info.I.isExternal;list (fun depth -> `Bool (I.shouldInline info I.defaultConfig depth)) [-1;0;2;3;4]] in
 tuple [
 list (fun n -> tuple [option int64 (C.tryLog2 n); option int64 (C.tryLog2UInt64 n)]) ints;
 list (fun f -> option int64 (C.tryTruncateFloatToInt64 f)) floats;
 list (fun op -> list (fun left -> list (fun right -> option J.aNF_cExpr (C.foldBinOp op left right)) atoms) atoms) ops;
 list (fun env -> list (fun op -> list (fun left -> list (fun right -> option J.aNF_cExpr (C.tryStrengthReduce env op left right)) atoms) atoms) ops) typeEnvs;
 list (fun op -> list (fun atom -> option J.aNF_cExpr (C.foldUnaryOp op atom)) atoms) [A.Neg;A.Not;A.BitNot];
 list (fun expr -> tuple [`Bool (E.mustPreserveEvaluation context expr); list J.aNF_tempId (E.TempSet.elements (E.cexprTempUses expr)); list J.aNF_tempId (List.rev (E.foldCExprTempIds (fun tid xs -> tid::xs) expr [])); list (fun tid -> `Bool (E.cexprUsesTemp (A.TempId tid) expr)) [0;3;4]]) all;
 list (fun env -> list (fun expr -> J.aNF_cExpr (S.substCExpr env expr)) all) envs;
 list (fun options -> list (fun env -> list (fun expr -> let expr,changed = S.optimizeCExpr context options env C.TempMap.empty tupleEnv expr in tuple [J.aNF_cExpr expr;`Bool changed]) all) envs) options;
 list (fun env -> list (fun atom -> `Bool (E.canForwardTupleElement context env atom)) atoms) typeEnvs;
 list (fun body -> let renamed,next = I.renameExpr (I.TempMap.of_list [A.TempId 3,A.TempId 30;A.TempId 4,A.TempId 40]) (A.VarGen 100) body in tuple [J.aNF_aExpr renamed;J.aNF_varGen next]) bodies;
 list (fun c -> J.aNF_cExpr (I.renameCExpr (I.TempMap.singleton (A.TempId 3) (A.TempId 30)) c)) fixtures;
 graphJson graph;graphJson (I.buildReverseCallGraph graph);
 list (fun calls -> list functionId (FS.elements calls)) (I.findSCCs ids graph);
 list (fun (_,info) -> infoJson info) (FunctionIdMap.toList (I.buildFunctionInfoMap functions));
 list (fun (_,info) -> infoJson info) (FunctionIdMap.toList (I.buildExternalCandidateInfoMap I.defaultConfig functions));
 graphJson (D.buildCallGraph functions);
 list J.aNF_functionDef (D.filterReachableFunctions (FS.singleton (AST.functionId 1L)) functions);
 list functionId (FS.elements (D.getReachableStdlib (D.buildCallGraph functions) [List.hd functions]))]
