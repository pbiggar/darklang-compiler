(* DirectCallFacts.fs - Value, call, and rewrite facts for SSA direct-call specialization. *)
[@@@warning "-4"]
module A = ANF
module TempMap = InliningCommon.TempMap
module M = TempMap
module IntSet = Set.Make (Int)
module FunctionSet = SpecializationIdentity.FunctionSet
type parameterRewrite = KeepParameter | ReplaceParameterWith of A.atom
type programAnalysis = {directCalls : A.atom list list FunctionIdMap.t; indirectTargets : FunctionSet.t}
type scalarLiteral = UnitScalar | IntScalar of A.sizedInt | BoolScalar of bool | FloatScalar of int64 | StringScalar of string
type knownValue = LiteralValue of scalarLiteral | Int128Value of AST.functionId * int64 * int64 | UInt128Value of AST.functionId * int64 * int64 | TupleValue of scalarLiteral list | RecordValue of A.recordDescriptor * scalarLiteral list
type literalPattern = (int * knownValue) list
type literalClone = {originalId : AST.functionId; cloneId : AST.functionId; cloneName : string; pattern : literalPattern}
type valueEnv = knownValue M.t
let nextCompare first next = if first = 0 then next () else first
let rec compareList compare left right = match left, right with [], [] -> 0 | [], _ -> -1 | _, [] -> 1 | left :: ls, right :: rs -> nextCompare (compare left right) (fun () -> compareList compare ls rs)
let compareSized left right =
 let rank = function A.Int8 _ -> 0 | A.Int16 _ -> 1 | A.Int32 _ -> 2 | A.Int64 _ -> 3 | A.UInt8 _ -> 4 | A.UInt16 _ -> 5 | A.UInt32 _ -> 6 | A.UInt64 _ -> 7 in
 nextCompare (Int.compare (rank left) (rank right)) (fun () -> match left, right with
  | A.Int8 left, A.Int8 right | A.Int16 left, A.Int16 right | A.UInt8 left, A.UInt8 right | A.UInt16 left, A.UInt16 right -> Int.compare left right
  | A.Int32 left, A.Int32 right -> Int32.compare left right
  | A.Int64 left, A.Int64 right | A.UInt32 left, A.UInt32 right -> Int64.compare left right
  | A.UInt64 left, A.UInt64 right -> Int64.unsigned_compare left right | _ -> 0)
let compareScalar left right =
 let rank = function UnitScalar -> 0 | IntScalar _ -> 1 | BoolScalar _ -> 2 | FloatScalar _ -> 3 | StringScalar _ -> 4 in
 nextCompare (Int.compare (rank left) (rank right)) (fun () -> match left, right with UnitScalar, UnitScalar -> 0 | IntScalar left, IntScalar right -> compareSized left right | BoolScalar left, BoolScalar right -> Bool.compare left right | FloatScalar left, FloatScalar right -> Int64.compare left right | StringScalar left, StringScalar right -> StringOrder.compare left right | _ -> 0)
let compareDescriptor left right =
 nextCompare (StringOrder.compare left.A.sourceTypeName right.A.sourceTypeName) (fun () -> nextCompare (StringOrder.compare left.A.runtimeTypeName right.A.runtimeTypeName) (fun () -> nextCompare (compareList AST.compareSemanticType left.A.typeArgs right.A.typeArgs) (fun () -> nextCompare (compareList (fun (ln, lt) (rn, rt) -> nextCompare (StringOrder.compare ln rn) (fun () -> AST.compareSemanticType lt rt)) left.A.fields right.A.fields) (fun () -> AST.compareSemanticType left.A.valueType right.A.valueType))))
let compareKnownValue left right =
 let rank = function LiteralValue _ -> 0 | Int128Value _ -> 1 | UInt128Value _ -> 2 | TupleValue _ -> 3 | RecordValue _ -> 4 in
 nextCompare (Int.compare (rank left) (rank right)) (fun () -> match left, right with
  | LiteralValue left, LiteralValue right -> compareScalar left right
  | Int128Value (lf, ll, lh), Int128Value (rf, rl, rh) | UInt128Value (lf, ll, lh), UInt128Value (rf, rl, rh) -> nextCompare (Int64.unsigned_compare (AST.functionIdValue lf) (AST.functionIdValue rf)) (fun () -> nextCompare (Int64.unsigned_compare ll rl) (fun () -> Int64.unsigned_compare lh rh))
  | TupleValue left, TupleValue right -> compareList compareScalar left right
  | RecordValue (ld, ls), RecordValue (rd, rs) -> nextCompare (compareDescriptor ld rd) (fun () -> compareList compareScalar ls rs)
  | _ -> 0)
let compareLiteralPattern = compareList (fun (li, lv) (ri, rv) -> nextCompare (Int.compare li ri) (fun () -> compareKnownValue lv rv))
(*
   Cloning is deliberately a small whole-program transform: without profile
   data, larger clone families are not justified by the saved scalar setup.
*)
let maxLiteralClonesPerFunction = 4
let maxLiteralClonesPerProgram = 16
let emptyAnalysis = {directCalls = FunctionIdMap.empty; indirectTargets = FunctionSet.empty}
(*
   A function reference already names the complete target, so retaining an
   indirect call would hide a direct-call specialization opportunity without
   preserving any dynamic dispatch. Closure calls remain indirect because their
   hidden capture argument uses a different calling convention.
*)
let exposeKnownIndirectCExpr = function A.IndirectCall (A.FuncRef func, args) -> A.Call (func, args) | A.IndirectTailCall (A.FuncRef func, args) -> A.TailCall (func, args) | operation -> operation
let addDirectCall name args analysis = {analysis with directCalls = FunctionIdMap.add name (args :: Option.value ~default:[] (FunctionIdMap.tryFind name analysis.directCalls)) analysis.directCalls}
let analyzeAtom atom analysis = match atom with A.FuncRef name -> {analysis with indirectTargets = FunctionSet.add name analysis.indirectTargets} | _ -> analysis
let analyzeAtoms atoms analysis = List.fold_left (fun state atom -> analyzeAtom atom state) analysis atoms
let analyzeCExpr operation analysis = match operation with
 | A.Atom atom | A.TypedAtom (atom, _) | A.UnaryPrim (_, atom) | A.RefCountInc (atom, _, _, _) | A.RefCountDec (atom, _, _, _) | A.Print (atom, _) | A.StdoutWrite (atom, _) | A.FileReadBlob atom | A.FileExists atom | A.FileDelete atom | A.FileCreateDirectory atom | A.FileSetExecutable atom | A.FloatSqrt atom | A.FloatAbs atom | A.FloatNeg atom | A.Int64ToFloat atom | A.FloatToInt64 atom | A.FloatToBits atom | A.RawAlloc atom | A.MappedAlloc atom | A.RawFree atom | A.MappedFree atom | A.RawGetByte (atom, _) | A.StringToRawPtr atom | A.RawPtrToString atom | A.BlobToRawPtr atom | A.RawPtrToBlob atom | A.RawPtrToInt128 atom | A.RawPtrToUInt128 atom | A.DictToRawPtr atom | A.ListToRawPtr atom | A.FixedBlockToRawPtr atom | A.RefCountIncString atom | A.RefCountDecString atom | A.RefCountIncBlob atom | A.RefCountDecBlob atom | A.RefCountIncInt atom | A.RefCountDecInt atom | A.FloatToString atom | A.Sleep atom | A.RuntimeErrorString atom -> analyzeAtom atom analysis
 | A.StringConcat (first, second, remaining) -> analyzeAtoms (first :: second :: remaining) analysis
 | A.Prim (_, left, right) | A.CanonicalBufferEq (_, left, right) | A.FileWriteBlob (left, right) | A.FileAppendText (left, right) | A.RawGet (left, right, _) | A.RawTake (left, right, _) | A.RawPtrToDict (left, right, _) | A.RawPtrToList (left, right, _) -> analyzeAtoms [left; right] analysis
 | A.IfValue (condition, yes, no) -> analyzeAtoms [condition; yes; no] analysis
 | A.Call (name, args) | A.BorrowedCall (name, args) | A.TailCall (name, args) -> analyzeAtoms args (addDirectCall name args analysis)
 | A.IndirectCall (func, args) | A.IndirectTailCall (func, args) | A.ClosureCall (func, args) | A.ClosureTailCall (func, args) -> analyzeAtoms (func :: args) analysis
 | A.ClosureAlloc (name, captures) -> analyzeAtoms captures {analysis with indirectTargets = FunctionSet.add name analysis.indirectTargets}
 | A.TupleAlloc atoms | A.RecordAlloc (_, atoms) | A.CliNative (_, atoms) -> analyzeAtoms atoms analysis
 | A.RecordClone (_, record, fields) | A.RecordReuse (_, _, record, fields) -> analyzeAtoms (record :: fields) analysis
 | A.TupleGet (tuple, _) | A.RecordGet (_, tuple, _) -> analyzeAtom tuple analysis
 | A.FileWriteFromPtr (first, second, third) | A.RawWriteWord (first, second, third) | A.RawWriteByte (first, second, third) | A.RawSlotInit (first, second, third, _) -> analyzeAtoms [first; second; third] analysis
 | A.RandomInt64 | A.DateTimeNow | A.StdinReadLine | A.RuntimeError _ -> analysis
let scalarLiteralAtom = function A.UnitLiteral -> Some UnitScalar | A.IntLiteral value -> Some (IntScalar value) | A.BoolLiteral value -> Some (BoolScalar value) | A.FloatLiteral value -> Some (FloatScalar (Int64.bits_of_float value)) | A.StringLiteral value -> Some (StringScalar value) | A.Var _ | A.FuncRef _ -> None
let atomForScalarLiteral = function UnitScalar -> A.UnitLiteral | IntScalar value -> A.IntLiteral value | BoolScalar value -> A.BoolLiteral value | FloatScalar bits -> A.FloatLiteral (Int64.float_of_bits bits) | StringScalar value -> A.StringLiteral value
let isScalarLiteralType = function AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16 | AST.TUInt32 | AST.TUInt64 | AST.TBool | AST.TFloat64 | AST.TString | AST.TChar | AST.TDateTime | AST.TUnit | AST.TSum _ -> true | _ -> false
let isConstructionValueType = function AST.TInt128 | AST.TUInt128 | AST.TRecord _ -> true | AST.TTuple fields -> List.length fields <= 3 | _ -> false
let isSpecializableValueType typ = isScalarLiteralType typ || isConstructionValueType typ
let scalarLiteralMatchesType typ value = match typ, value with
 | AST.TUnit, UnitScalar | AST.TInt8, IntScalar (A.Int8 _) | AST.TInt16, IntScalar (A.Int16 _) | AST.TInt32, IntScalar (A.Int32 _) | AST.TInt64, IntScalar (A.Int64 _) | AST.TUInt8, IntScalar (A.UInt8 _) | AST.TUInt16, IntScalar (A.UInt16 _) | AST.TUInt32, IntScalar (A.UInt32 _) | AST.TUInt64, IntScalar (A.UInt64 _) | AST.TBool, BoolScalar _ | AST.TFloat64, FloatScalar _ | AST.TString, StringScalar _ | AST.TChar, StringScalar _ | AST.TDateTime, IntScalar (A.Int64 _) | AST.TSum _, IntScalar (A.Int64 _) -> true | _ -> false
let knownValueMatchesType typ value = match typ, value with
 | _, LiteralValue literal -> scalarLiteralMatchesType typ literal
 | AST.TInt128, Int128Value _ | AST.TUInt128, UInt128Value _ -> true
 | AST.TTuple types, TupleValue fields -> List.length types = List.length fields && List.for_all2 scalarLiteralMatchesType types fields
 | AST.TRecord (name, _), RecordValue (descriptor, fields) -> (name = descriptor.A.sourceTypeName || name = descriptor.A.runtimeTypeName) && List.length descriptor.A.fields = List.length fields && List.for_all2 scalarLiteralMatchesType (List.map snd descriptor.A.fields) fields
 | AST.TSum _, TupleValue [_; _] -> true | _ -> false
let rewriteAtom substitutions = function A.Var id as atom -> Option.value ~default:atom (M.find_opt id substitutions) | atom -> atom
let rewriteCallArgs rewrites name args = match FunctionIdMap.tryFind name rewrites with
 | None -> args
 | Some rewrites ->
   let rec loop rewrites args reversed = match rewrites, args with [], [] -> List.rev reversed | rewrite :: rs, arg :: args -> (match rewrite with KeepParameter -> loop rs args (arg :: reversed) | ReplaceParameterWith _ -> loop rs args reversed) | _ -> Crash.crash ("Direct-call argument count mismatch for '" ^ StructuralFormat.format (AST.DiagnosticFormatting.func name) ^ "'") in loop rewrites args []
let rewriteCExpr rewrites substitutions operation =
 match operation with
 | A.Call (name, args) -> A.Call (name, rewriteCallArgs rewrites name (List.map (rewriteAtom substitutions) args))
 | A.BorrowedCall (name, args) -> A.BorrowedCall (name, rewriteCallArgs rewrites name (List.map (rewriteAtom substitutions) args))
 | A.TailCall (name, args) -> A.TailCall (name, rewriteCallArgs rewrites name (List.map (rewriteAtom substitutions) args))
 | A.RawGetByte (ptr, offset) -> A.RawGetByte (rewriteAtom substitutions ptr, offset)
 | _ -> match ANFSubstitution.substCExpr substitutions operation with A.CanonicalBufferEq (_, A.StringLiteral left, A.StringLiteral right) -> A.Atom (A.BoolLiteral (left = right)) | operation -> operation
let knownValueForAtom env atom = match scalarLiteralAtom atom with Some value -> Some (LiteralValue value) | None -> (match atom with A.Var id -> M.find_opt id env | _ -> None)
let knownLiteralsForAtoms env atoms =
 let rec loop reversed = function [] -> Some (List.rev reversed) | atom :: rest -> (match knownValueForAtom env atom with Some (LiteralValue literal) -> loop (literal :: reversed) rest | _ -> None) in loop [] atoms
let knownValueForCExpr functionNames env = function
 | A.Atom atom | A.TypedAtom (atom, _) -> knownValueForAtom env atom
 | A.Call (name, [A.IntLiteral (A.UInt64 low); A.IntLiteral (A.UInt64 high)]) -> (match FunctionIdMap.tryFind name functionNames with Some "Darklang.Stdlib.Int128.__fromWords" -> Some (Int128Value (name, low, high)) | Some "Darklang.Stdlib.UInt128.__fromWords" -> Some (UInt128Value (name, low, high)) | _ -> None)
 | A.TupleAlloc atoms when List.length atoms <= 3 -> Option.map (fun values -> TupleValue values) (knownLiteralsForAtoms env atoms)
 | A.RecordAlloc (descriptor, atoms) when List.length atoms <= 3 -> Option.map (fun values -> RecordValue (descriptor, values)) (knownLiteralsForAtoms env atoms)
 | _ -> None
let addKnownBinding names id operation env = match knownValueForCExpr names env operation with Some value -> M.add id value env | None -> M.remove id env
let addKnownCall name args env calls = FunctionIdMap.add name (List.map (knownValueForAtom env) args :: Option.value ~default:[] (FunctionIdMap.tryFind name calls)) calls
let literalPatternAt eligible values = List.filter_map (fun (index, value) -> if IntSet.mem index eligible then Option.map (fun value -> index, value) value else None) (List.mapi (fun index value -> index, value) values)
let boundedCloneGroups groups =
 let selected, _ = List.fold_left (fun (selected, remaining) (id, name, patterns) -> let count = List.length patterns in if count <= remaining then (id, name, patterns) :: selected, remaining - count else selected, remaining) ([], maxLiteralClonesPerProgram) groups in List.rev selected
let buildLiteralClones existingIds existingNames groups =
 let specs = List.concat_map (fun (id, name, patterns) -> List.mapi (fun index pattern -> id, name ^ "__literal_" ^ string_of_int index, pattern) patterns) groups in
 let names = List.map (fun (_, name, _) -> name) specs in
 if List.length names = List.length (List.sort_uniq StringOrder.compare names) && List.for_all (fun name -> not (StringOrder.Set.mem name existingNames)) names then
 let allocated = AST.allocateFunctionIds existingIds (List.to_seq names) in
 List.map (fun (originalId, cloneName, pattern) -> let cloneId = match StringOrder.Map.find_opt cloneName allocated with Some id -> id | None -> Crash.crash "Literal clone identity was not allocated" in {originalId; cloneId; cloneName; pattern}) specs else []
let removePatternArguments pattern args = let removed = IntSet.of_list (List.map fst pattern) in List.filter_map (fun (index, arg) -> if IntSet.mem index removed then None else Some arg) (List.mapi (fun index arg -> index, arg) args)
let tryItem index values = if index < 0 then None else List.nth_opt values index
let routeDirectCall clones env name args =
 let matches pattern = List.for_all (fun (index, value) -> Option.bind (tryItem index args) (knownValueForAtom env) = Some value) pattern in
 match Option.bind (FunctionIdMap.tryFind name clones) (List.find_opt (fun clone -> matches clone.pattern)) with Some clone -> clone.cloneId, removePatternArguments clone.pattern args | None -> name, args
let routeCExpr clones env = function A.Call (name, args) -> let name, args = routeDirectCall clones env name args in A.Call (name, args) | A.BorrowedCall (name, args) -> let name, args = routeDirectCall clones env name args in A.BorrowedCall (name, args) | A.TailCall (name, args) -> let name, args = routeDirectCall clones env name args in A.TailCall (name, args) | operation -> operation
let cexprForKnownValue = function LiteralValue literal -> A.Atom (atomForScalarLiteral literal) | Int128Value (name, low, high) | UInt128Value (name, low, high) -> A.Call (name, [A.IntLiteral (A.UInt64 low); A.IntLiteral (A.UInt64 high)]) | TupleValue fields -> A.TupleAlloc (List.map atomForScalarLiteral fields) | RecordValue (descriptor, fields) -> A.RecordAlloc (descriptor, List.map atomForScalarLiteral fields)
let isRematerializedValue names operation = Option.is_some (knownValueForCExpr names M.empty operation)
