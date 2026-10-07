(*
   Optimize the CExpr
   Check for copy propagation: if cexpr is just an Atom, substitute it
   Copy propagation: don't emit binding, just substitute
   Constant propagation
   Optimize the body
   Dead code elimination: discard an unused binding only when evaluating
   it is not required for effects or ownership bookkeeping.
   Copy propagation: skip this binding entirely
   Dead code elimination
   Fold constant conditions
*)
(* ANFExpressionOptimization.ml - Propagate facts and common expressions through lexical ANF control flow. *)
[@@@warning "-4"]
open ANF
open ANFConstants
open ANFEffects
open ANFSubstitution
module TM = TempMap
module TS = TempSet
type optimizeAExprResult = {expr : aExpr; changed : bool; uses : TS.t}
type scalarUnaryCSEOp = PrimitiveUnary of unaryOp | FloatSqrtOp | FloatAbsOp | FloatNegOp | Int64ToFloatOp | FloatToInt64Op | FloatToBitsOp
type cSEKey = BinaryValue of binOp * atom * atom | UnaryValue of scalarUnaryCSEOp * atom | ConditionalValue of atom * atom * atom | TupleProjection of atom * int | RecordProjection of recordDescriptor * atom * int
let compareSizedInt a b =
 let rank = function Int8 _ -> 0 | Int16 _ -> 1 | Int32 _ -> 2 | Int64 _ -> 3 | UInt8 _ -> 4 | UInt16 _ -> 5 | UInt32 _ -> 6 | UInt64 _ -> 7 in
 let rankOrder = Int.compare (rank a) (rank b) in
 if rankOrder <> 0 then rankOrder else match a,b with
 | Int8 a,Int8 b | Int16 a,Int16 b | UInt8 a,UInt8 b | UInt16 a,UInt16 b -> Int.compare a b
 | Int32 a,Int32 b -> Int32.compare a b
 | Int64 a,Int64 b | UInt32 a,UInt32 b -> Int64.compare a b
 | UInt64 a,UInt64 b -> Int64.unsigned_compare a b
 | _ -> assert false
let compareAtom a b =
 let rank = function UnitLiteral -> 0 | IntLiteral _ -> 1 | BoolLiteral _ -> 2 | StringLiteral _ -> 3 | FloatLiteral _ -> 4 | Var _ -> 5 | FuncRef _ -> 6 in
 let rankOrder = Int.compare (rank a) (rank b) in
 if rankOrder <> 0 then rankOrder else match a,b with
 | UnitLiteral,UnitLiteral -> 0 | IntLiteral a,IntLiteral b -> compareSizedInt a b
 | BoolLiteral a,BoolLiteral b -> Bool.compare a b | StringLiteral a,StringLiteral b -> StringOrder.compare a b
 | FloatLiteral a,FloatLiteral b -> Float.compare a b
 | Var (TempId a),Var (TempId b) -> Int.compare a b
 | FuncRef a,FuncRef b -> Int64.unsigned_compare (AST.functionIdValue a) (AST.functionIdValue b)
 | _ -> assert false
let rec compareList cmp xs ys = match xs,ys with
 | [],[] -> 0 | [],_ -> -1 | _,[] -> 1 | x::xs,y::ys -> let order = cmp x y in if order = 0 then compareList cmp xs ys else order
let thenCompare first next = if first = 0 then next () else first
let compareDescriptor a b =
 thenCompare (StringOrder.compare a.sourceTypeName b.sourceTypeName) (fun () ->
 thenCompare (StringOrder.compare a.runtimeTypeName b.runtimeTypeName) (fun () ->
 thenCompare (compareList AST.compareSemanticType a.typeArgs b.typeArgs) (fun () ->
 thenCompare (compareList (fun (an,at) (bn,bt) -> thenCompare (StringOrder.compare an bn) (fun () -> AST.compareSemanticType at bt)) a.fields b.fields) (fun () -> AST.compareSemanticType a.valueType b.valueType))))
let compareUnary a b =
 let rank = function PrimitiveUnary _ -> 0 | FloatSqrtOp -> 1 | FloatAbsOp -> 2 | FloatNegOp -> 3 | Int64ToFloatOp -> 4 | FloatToInt64Op -> 5 | FloatToBitsOp -> 6 in
 thenCompare (Int.compare (rank a) (rank b)) (fun () -> match a,b with PrimitiveUnary a,PrimitiveUnary b -> Stdlib.compare a b | _ -> 0)
let compareKey a b =
 let rank = function BinaryValue _ -> 0 | UnaryValue _ -> 1 | ConditionalValue _ -> 2 | TupleProjection _ -> 3 | RecordProjection _ -> 4 in
 thenCompare (Int.compare (rank a) (rank b)) (fun () -> match a,b with
 | BinaryValue (op,a,b),BinaryValue (op',a',b') -> thenCompare (Stdlib.compare op op') (fun () -> thenCompare (compareAtom a a') (fun () -> compareAtom b b'))
 | UnaryValue (op,a),UnaryValue (op',a') -> thenCompare (compareUnary op op') (fun () -> compareAtom a a')
 | ConditionalValue (a,b,c),ConditionalValue (a',b',c') -> thenCompare (compareAtom a a') (fun () -> thenCompare (compareAtom b b') (fun () -> compareAtom c c'))
 | TupleProjection (a,index),TupleProjection (b,index') -> thenCompare (compareAtom a b) (fun () -> Int.compare index index')
 | RecordProjection (d,a,index),RecordProjection (d',b,index') -> thenCompare (compareDescriptor d d') (fun () -> thenCompare (compareAtom a b) (fun () -> Int.compare index index'))
 | _ -> assert false)
module CSEnv = Map.Make (struct type t = cSEKey let compare = compareKey end)
let isCommutativeBinOp = function Add | Mul | Eq | Neq | And | Or | BitAnd | BitOr | BitXor -> true | Sub | Div | Mod | Lt | Gt | Lte | Gte | Shl | Shr -> false
(*
   Canonicalize relational comparisons to their less-than spelling so
   reversing both the operator and operands produces the same CSE key.
*)
let binaryCSEKey op left right = match op with
 | Gt -> BinaryValue (Lt,right,left) | Gte -> BinaryValue (Lte,right,left)
 | _ when isCommutativeBinOp op && compareAtom right left < 0 -> BinaryValue (op,right,left)
 | _ -> BinaryValue (op,left,right)
let isRecordProjectionCSEType = function
 | AST.TInt64 | AST.TInt32 | AST.TInt16 | AST.TInt8 | AST.TUInt64 | AST.TUInt32 | AST.TUInt16 | AST.TUInt8 | AST.TBool | AST.TChar | AST.TDateTime -> true
 | AST.TInt128 | AST.TInt | AST.TUInt128 | AST.TFloat64 | AST.TString | AST.TBlob | AST.TUnit | AST.TNever | AST.TFunction _ | AST.TTuple _ | AST.TRecord _ | AST.TSum _ | AST.TList _ | AST.TStream _ | AST.TVar _ | AST.TInferenceVar _ | AST.TInternalRawPtr | AST.TDict _ -> false
(*
   Return a value-numbering key only when merging two evaluations preserves
   allocation identity, mutable-memory observations, and ownership semantics.
   Keep this match exhaustive so every new CExpr case requires an explicit CSE
   decision instead of silently falling through a permissive purity test.
*)
let tryCSEKey = function
 | Prim (op,left,right) -> Some (binaryCSEKey op left right)
 | UnaryPrim (op,atom) -> Some (UnaryValue (PrimitiveUnary op,atom))
 | IfValue (condition,yes,no) -> Some (ConditionalValue (condition,yes,no))
 | FloatSqrt atom -> Some (UnaryValue (FloatSqrtOp,atom)) | FloatAbs atom -> Some (UnaryValue (FloatAbsOp,atom)) | FloatNeg atom -> Some (UnaryValue (FloatNegOp,atom))
 | Int64ToFloat atom -> Some (UnaryValue (Int64ToFloatOp,atom)) | FloatToInt64 atom -> Some (UnaryValue (FloatToInt64Op,atom)) | FloatToBits atom -> Some (UnaryValue (FloatToBitsOp,atom))
 | TupleGet (tuple,index) -> Some (TupleProjection (tuple,index))
 | RecordGet (descriptor,record,index) ->
  (match if index < 0 then None else List.nth_opt descriptor.fields index with
  | Some (_,typ) when isRecordProjectionCSEType typ -> Some (RecordProjection (descriptor,record,index))
  | Some _ -> None | None -> Crash.crash ("ANF CSE: invalid field index " ^ string_of_int index ^ " for record " ^ descriptor.runtimeTypeName))
 | Atom _ | TypedAtom _ | Call _ | BorrowedCall _ | TailCall _ | IndirectCall _ | IndirectTailCall _ | ClosureAlloc _ | ClosureCall _ | ClosureTailCall _ | TupleAlloc _ | RecordAlloc _ | RecordClone _ | RecordReuse _ | StringConcat _ | CanonicalBufferEq _ | RefCountInc _ | RefCountDec _ | Print _ | StdoutWrite _ | StdinReadLine | RuntimeError _ | RuntimeErrorString _ | FileReadBlob _ | FileExists _ | FileWriteBlob _ | FileAppendText _ | FileDelete _ | FileCreateDirectory _ | FileSetExecutable _ | FileWriteFromPtr _ | RawAlloc _ | MappedAlloc _ | RawFree _ | MappedFree _ | RawGet _ | RawTake _ | RawGetByte _ | RawWriteWord _ | RawWriteByte _ | RawSlotInit _ | StringToRawPtr _ | RawPtrToString _ | BlobToRawPtr _ | RawPtrToBlob _ | RawPtrToInt128 _ | RawPtrToUInt128 _ | DictToRawPtr _ | RawPtrToDict _ | ListToRawPtr _ | FixedBlockToRawPtr _ | RawPtrToList _ | RefCountIncString _ | RefCountDecString _ | RefCountIncBlob _ | RefCountDecBlob _ | RefCountIncInt _ | RefCountDecInt _ | RandomInt64 | DateTimeNow | Sleep _ | CliNative _ | FloatToString _ -> None
let tryAbsorbedAtom outer left right = if outer = left || outer = right then Some outer else None
let rec aExprUsesTemp tid = function
 | Jump (_,atom) | Return atom -> atomUsesTemp tid atom
 | Join (parameter,continuation,entry) -> aExprUsesTemp tid entry || (parameter.id <> tid && aExprUsesTemp tid continuation)
 | Let (_,cexpr,body) -> cexprUsesTemp tid cexpr || aExprUsesTemp tid body
 | If (condition,yes,no) -> atomUsesTemp tid condition || aExprUsesTemp tid yes || aExprUsesTemp tid no
(*
   Replace uses of one branch-local binding with a shared binding. The bound
   TempId is removed from the substitution when crossing a shadowing Let so
   this remains correct for hand-built ANF as well as globally fresh output.
*)
let rec replaceTempUses sourceTid replacement expr =
 let substitution = TM.singleton sourceTid replacement in
 match expr with
 | Jump (target,atom) -> Jump (target,substAtom substitution atom)
 | Join (parameter,continuation,entry) -> let body = if parameter.id = sourceTid then continuation else replaceTempUses sourceTid replacement continuation in Join (parameter,body,replaceTempUses sourceTid replacement entry)
 | Return atom -> Return (substAtom substitution atom)
 | Let (tid,cexpr,body) -> let cexpr' = substCExpr substitution cexpr in let body' = if tid = sourceTid then body else replaceTempUses sourceTid replacement body in Let (tid,cexpr',body')
 | If (condition,yes,no) -> If (substAtom substitution condition,replaceTempUses sourceTid replacement yes,replaceTempUses sourceTid replacement no)
let rec aExprMustPreserveEvaluation context = function
 | Jump _ | Return _ -> false
 | Join (_,continuation,entry) -> aExprMustPreserveEvaluation context continuation || aExprMustPreserveEvaluation context entry
 | Let (_,cexpr,body) -> mustPreserveEvaluation context cexpr || aExprMustPreserveEvaluation context body
 | If (_,yes,no) -> aExprMustPreserveEvaluation context yes || aExprMustPreserveEvaluation context no
(*
   Hoist before the binding that computes a local condition so the shared
   expression does not separate a comparison from its branch during lowering.
*)
let tryHoistSharedLeadingBranchBinding context (options : optimizeOptions) expr =
 if not options.enableCSE then None else match expr with
 | Let (condTid,condCExpr,If (Var ifCondTid,Let (thenTid,thenCExpr,thenBody),Let (elseTid,elseCExpr,elseBody)))
   when ifCondTid=condTid && thenCExpr=elseCExpr && not (mustPreserveEvaluation context condCExpr) && not (mustPreserveEvaluation context thenCExpr) && not (aExprMustPreserveEvaluation context thenBody) && not (aExprMustPreserveEvaluation context elseBody) && not (cexprUsesTemp condTid thenCExpr) ->
  let elseBody' = replaceTempUses elseTid (Var thenTid) elseBody in
  let conditional = If (Var condTid,thenBody,elseBody') in Some (Let (thenTid,thenCExpr,Let (condTid,condCExpr,conditional)))
 | _ -> None
let tryComplementIntegerComparison = function Eq -> Some Neq | Neq -> Some Eq | Lt -> Some Gte | Gt -> Some Lte | Lte -> Some Gt | Gte -> Some Lt | _ -> None
(*
   The converted index always fits in Int64. Call the list primitive
   with that index directly, preserving getAt's bounds check.
*)
let trySimplifyAdjacentLet (context : optimizeContext) typeEnv tid cexpr body =
 let hasName id name = FunctionIdMap.tryFind id context.functionNames = Some name in
 let resolve name = match StringOrder.Map.find_opt name context.functionIds with Some id -> id | None -> Crash.crash ("ANF optimization helper '" ^ name ^ "' is absent from registries") in
 match cexpr,body with
 | Call (fromInt64Id,[nativeIndex]),Let (resultTid,Call (getAtId,[listValue;Var indexTid]),resultBody)
  when hasName fromInt64Id "Darklang.Stdlib.Int.fromInt64" && Option.fold ~none:false ~some:(fun name -> name="Darklang.Stdlib.List.getAt" || String.starts_with ~prefix:"Darklang.Stdlib.List.getAt_" name) (FunctionIdMap.tryFind getAtId context.functionNames) && indexTid=tid && not (aExprUsesTemp tid resultBody) ->
  let internalGetAt = Option.bind (FunctionIdMap.tryFind getAtId context.functionNames) (fun name ->
   let publicPrefix = "Darklang.Stdlib.List.getAt" in
   if name=publicPrefix || String.starts_with ~prefix:(publicPrefix ^ "_") name then
    let internalName = "Darklang.Stdlib.List.__getAt" ^ String.sub name (String.length publicPrefix) (String.length name-String.length publicPrefix) in
    StringOrder.Map.find_opt internalName context.functionIds else None) in
  Option.map (fun target -> Let (resultTid,Call (target,[listValue;nativeIndex]),resultBody)) internalGetAt
    | Call (fromInt64Id, [nativeIndex]),
      Let (
          resultTid,
          Call (getByteAtId, [value; Var indexTid]),
          resultBody
      )
        when hasName fromInt64Id "Darklang.Stdlib.Int.fromInt64"
             && hasName getByteAtId "Darklang.Stdlib.String.getByteAt"
             && indexTid = tid
             && not (aExprUsesTemp tid resultBody) ->
        Some (
            Let (
                resultTid,
                Call (
                    resolve "Darklang.Stdlib.String.__getByteAtInt64",
                    [value; nativeIndex]
                ),
                resultBody
            )
        )
    | Call (getByteAtInt64Id, [value; index]),
      Let (
          tagTid,
          TupleGet (Var optionTid, 0),
          Let (
              conditionTid,
              Prim (Eq, Var projectedTagTid, IntLiteral (Int64 0L)),
              If (Var branchConditionTid, someBranch, noneBranch)
          )
      )
        when hasName getByteAtInt64Id "Darklang.Stdlib.String.__getByteAtInt64"
             && optionTid = tid
             && projectedTagTid = tagTid
             && branchConditionTid = conditionTid
             && not (aExprUsesTemp tid someBranch)
             && not (aExprUsesTemp tid noneBranch)
             && not (aExprUsesTemp tagTid someBranch)
             && not (aExprUsesTemp tagTid noneBranch)
             && not (aExprUsesTemp conditionTid someBranch)
             && not (aExprUsesTemp conditionTid noneBranch) ->
        (*  When the Some payload is dead, materializing Option<UInt8> only to *)
        (*  inspect its tag is equivalent to the byte-index bounds check. *)
        Some (
            Let (
                tid,
                Call (resolve "Darklang.Stdlib.String.__byteLength", [value]),
                Let (
                    tagTid,
                    Prim (Gte, index, IntLiteral (Int64 0L)),
                    If (
                        Var tagTid,
                        Let (
                            conditionTid,
                            Prim (Lt, index, Var tid),
                            If (Var conditionTid, someBranch, noneBranch)
                        ),
                        noneBranch
                    )
                )
            )
        )
    | Call (getByteAtInt64Id, [value; index]),
      Let (
          conditionTid,
          Prim (Neq, Var optionTid, IntLiteral (Int64 256L)),
          If (Var branchConditionTid, someBranch, noneBranch)
      )
        when hasName getByteAtInt64Id "Darklang.Stdlib.String.__getByteAtInt64"
             && optionTid = tid
             && branchConditionTid = conditionTid
             && not (aExprUsesTemp tid someBranch)
             && not (aExprUsesTemp tid noneBranch)
             && not (aExprUsesTemp conditionTid someBranch)
             && not (aExprUsesTemp conditionTid noneBranch) ->
        (*  An unused Some byte only needs the index bounds check; the spare *)
        (*  UInt8 value 256 is never a valid byte. The inner Bool rebinds the *)
        (*  condition only after the outer branch consumed its old value. *)
        Some (
            Let (
                tid,
                Call (resolve "Darklang.Stdlib.String.__byteLength", [value]),
                Let (
                    conditionTid,
                    Prim (Gte, index, IntLiteral (Int64 0L)),
                    If (
                        Var conditionTid,
                        Let (
                            conditionTid,
                            Prim (Lt, index, Var tid),
                            If (Var conditionTid, someBranch, noneBranch)
                        ),
                        noneBranch
                    )
                )
            )
        )
    | Call (getByteAtInt64Id, [value; index]),
      Let (
          conditionTid,
          Prim (Neq, Var optionTid, IntLiteral (Int64 256L)),
          If (
              Var branchConditionTid,
              Let (payloadTid, TypedAtom (Var payloadOptionTid, AST.TUInt8), payloadBody),
              noneBranch
          )
      )
        when hasName getByteAtInt64Id "Darklang.Stdlib.String.__getByteAtInt64"
             && optionTid = tid
             && branchConditionTid = conditionTid
             && payloadOptionTid = tid
             && not (aExprUsesTemp tid payloadBody)
             && not (aExprUsesTemp tid noneBranch)
             && not (aExprUsesTemp conditionTid payloadBody)
             && not (aExprUsesTemp conditionTid noneBranch) ->
        let loadedPayload =
            Let (
                payloadTid,
                Call (resolve "Darklang.Stdlib.String.__byteAtUnchecked", [value; index]),
                payloadBody
            ) in
        Some (
            Let (
                tid,
                Call (resolve "Darklang.Stdlib.String.__byteLength", [value]),
                Let (
                    conditionTid,
                    Prim (Gte, index, IntLiteral (Int64 0L)),
                    If (
                        Var conditionTid,
                        Let (
                            conditionTid,
                            Prim (Lt, index, Var tid),
                            If (Var conditionTid, loadedPayload, noneBranch)
                        ),
                        noneBranch
                    )
                )
            )
        )
    | Call (getByteAtInt64Id, [value; index]),
      Let (
          tagTid,
          TupleGet (Var optionTid, 0),
          Let (
              conditionTid,
              Prim (Eq, Var projectedTagTid, IntLiteral (Int64 0L)),
              If (
                  Var branchConditionTid,
                  Let (payloadTid, TupleGet (Var payloadOptionTid, 1), payloadBody),
                  noneBranch
              )
          )
      )
        when hasName getByteAtInt64Id "Darklang.Stdlib.String.__getByteAtInt64"
             && optionTid = tid
             && projectedTagTid = tagTid
             && branchConditionTid = conditionTid
             && payloadOptionTid = tid
             && not (aExprUsesTemp tid payloadBody)
             && not (aExprUsesTemp tid noneBranch)
             && not (aExprUsesTemp tagTid payloadBody)
             && not (aExprUsesTemp tagTid noneBranch)
             && not (aExprUsesTemp conditionTid payloadBody)
             && not (aExprUsesTemp conditionTid noneBranch) ->
        (*  Preserve the Option match's control flow while replacing its boxed *)
        (*  payload with the unchecked byte load guarded by the same bounds. *)
        let loadedPayload =
            Let (
                payloadTid,
                Call (resolve "Darklang.Stdlib.String.__byteAtUnchecked", [value; index]),
                payloadBody
            ) in
        Some (
            Let (
                tid,
                Call (resolve "Darklang.Stdlib.String.__byteLength", [value]),
                Let (
                    tagTid,
                    Prim (Gte, index, IntLiteral (Int64 0L)),
                    If (
                        Var tagTid,
                        Let (
                            conditionTid,
                            Prim (Lt, index, Var tid),
                            If (Var conditionTid, loadedPayload, noneBranch)
                        ),
                        noneBranch
                    )
                )
            )
        )
    | UnaryPrim (Not, source), If (Var conditionTid, thenBranch, elseBranch)
        when conditionTid = tid
             && not (aExprUsesTemp tid thenBranch)
             && not (aExprUsesTemp tid elseBranch) ->
        Some (If (source, elseBranch, thenBranch))
    | Prim (op, left, right), Let (notTid, UnaryPrim (Not, Var sourceTid), notBody)
        when sourceTid = tid
             && isIntegerAtom typeEnv left
             && isIntegerAtom typeEnv right
             && not (aExprUsesTemp tid notBody) ->
        (*  Ordered integer comparisons have exact complements. Float relations *)
        (*  do not: both x < NaN and x >= NaN are false. *)
        tryComplementIntegerComparison op
        |> (fun value -> Option.map (fun complement -> Let (notTid, Prim (complement, left, right), notBody)) value)
    | UnaryPrim (Neg, negated), Let (resultTid, Prim (Add, other, Var negatedTid), resultBody) when negatedTid = tid
             && not (atomUsesTemp tid other)
             && not (aExprUsesTemp tid resultBody)
             && isInt64Atom typeEnv negated
             && isInt64Atom typeEnv other ->
        Some (Let (resultTid, Prim (Sub, other, negated), resultBody))
    | UnaryPrim (Neg, negated), Let (resultTid, Prim (Add, Var negatedTid, other), resultBody) when negatedTid = tid
             && not (atomUsesTemp tid other)
             && not (aExprUsesTemp tid resultBody)
             && isInt64Atom typeEnv negated
             && isInt64Atom typeEnv other ->
        Some (Let (resultTid, Prim (Sub, other, negated), resultBody))
    | Prim (Add, source, IntLiteral (Int64 a)),
      Let (addTid, Prim (Add, Var sourceTid, IntLiteral (Int64 b)), addBody)
        when sourceTid = tid ->
        (*  Keep the inner binding for this rewrite; the recursive optimization *)
        (*  removes it only when the reassociated expression was its final use. *)
        let combined = IntLiteral (Int64 (Int64.add a b)) in
        Some (Let (tid, cexpr, Let (addTid, Prim (Add, source, combined), addBody)))
    | Prim (Mul, source, IntLiteral (Int64 a)),
      Let (multiplyTid, Prim (Mul, Var sourceTid, IntLiteral (Int64 b)), multiplyBody)
        when sourceTid = tid ->
        (*  Int64 multiplication is associative modulo 2^64. Keeping the inner *)
        (*  binding here lets the recursive liveness pass remove it only when *)
        (*  the reassociated expression was its final use. *)
        let combined = IntLiteral (Int64 (Int64.mul a b)) in
        Some (Let (tid, cexpr, Let (multiplyTid, Prim (Mul, source, combined), multiplyBody)))
    | Prim (Mul, source, IntLiteral (Int64 coefficient)),
      Let (resultTid, Prim (Add, Var productTid, outerSource), resultBody) when productTid = tid
             && source = outerSource
             && isInt64Atom typeEnv source ->
        (*  Int64 arithmetic wraps modulo 2^64, so c*x + x = (c+1)*x. *)
        (*  Retain the product until recursive liveness cleanup proves it dead. *)
        let combined = IntLiteral (Int64 (Int64.add coefficient 1L)) in
        Some (Let (tid, cexpr, Let (resultTid, Prim (Mul, source, combined), resultBody)))
    | Prim (Mul, source, IntLiteral (Int64 coefficient)),
      Let (resultTid, Prim (Add, outerSource, Var productTid), resultBody) when productTid = tid
             && source = outerSource
             && isInt64Atom typeEnv source ->
        (*  Int64 arithmetic wraps modulo 2^64, so c*x + x = (c+1)*x. *)
        (*  Retain the product until recursive liveness cleanup proves it dead. *)
        let combined = IntLiteral (Int64 (Int64.add coefficient 1L)) in
        Some (Let (tid, cexpr, Let (resultTid, Prim (Mul, source, combined), resultBody)))
    | Prim (Mul, IntLiteral (Int64 coefficient), source),
      Let (resultTid, Prim (Add, Var productTid, outerSource), resultBody) when productTid = tid
             && source = outerSource
             && isInt64Atom typeEnv source ->
        (*  Int64 arithmetic wraps modulo 2^64, so c*x + x = (c+1)*x. *)
        (*  Retain the product until recursive liveness cleanup proves it dead. *)
        let combined = IntLiteral (Int64 (Int64.add coefficient 1L)) in
        Some (Let (tid, cexpr, Let (resultTid, Prim (Mul, source, combined), resultBody)))
    | Prim (Mul, IntLiteral (Int64 coefficient), source),
      Let (resultTid, Prim (Add, outerSource, Var productTid), resultBody) when productTid = tid
             && source = outerSource
             && isInt64Atom typeEnv source ->
        (*  Int64 arithmetic wraps modulo 2^64, so c*x + x = (c+1)*x. *)
        (*  Retain the product until recursive liveness cleanup proves it dead. *)
        let combined = IntLiteral (Int64 (Int64.add coefficient 1L)) in
        Some (Let (tid, cexpr, Let (resultTid, Prim (Mul, source, combined), resultBody)))
    | Prim (Add, source, cancelled),
      Let (resultTid, Prim (Sub, Var intermediateTid, outerCancelled), resultBody)
        when intermediateTid = tid
             && cancelled = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv cancelled ->
        Some (Let (tid, cexpr, Let (resultTid, Atom source, resultBody)))
    | Prim (Add, source, remaining),
      Let (resultTid, Prim (Sub, Var intermediateTid, outerCancelled), resultBody)
        when intermediateTid = tid
             && source = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv remaining ->
        Some (Let (tid, cexpr, Let (resultTid, Atom remaining, resultBody)))
    | Prim (Sub, source, cancelled),
      Let (resultTid, Prim (Add, Var intermediateTid, outerCancelled), resultBody)
        when intermediateTid = tid
             && cancelled = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv cancelled ->
        Some (Let (tid, cexpr, Let (resultTid, Atom source, resultBody)))
    | Prim (Sub, source, cancelled),
      Let (resultTid, Prim (Add, outerCancelled, Var intermediateTid), resultBody)
        when intermediateTid = tid
             && cancelled = outerCancelled
             && isInt64Atom typeEnv source
             && isInt64Atom typeEnv cancelled ->
        Some (Let (tid, cexpr, Let (resultTid, Atom source, resultBody)))
    | UnaryPrim (Not, source), Let (notTid, UnaryPrim (Not, Var sourceTid), notBody)
        when sourceTid = tid ->
        Some (Let (notTid, Atom source, notBody))
    | UnaryPrim (BitNot, source), Let (notTid, UnaryPrim (BitNot, Var sourceTid), notBody)
        when sourceTid = tid ->
        Some (Let (notTid, Atom source, notBody))
    | UnaryPrim (Neg, source), Let (negTid, UnaryPrim (Neg, Var sourceTid), negBody)
        when sourceTid = tid ->
        Some (Let (negTid, Atom source, negBody))
    | FloatNeg source, Let (negTid, FloatNeg (Var sourceTid), negBody)
        when sourceTid = tid ->
        Some (Let (negTid, Atom source, negBody))
    | FloatAbs source, Let (absTid, FloatAbs (Var sourceTid), absBody)
        when sourceTid = tid ->
        Some (Let (absTid, FloatAbs source, absBody))
    | FloatNeg source, Let (absTid, FloatAbs (Var sourceTid), absBody)
        when sourceTid = tid ->
        Some (Let (absTid, FloatAbs source, absBody))
    | Prim (Or, nestedLeft, nestedRight), Let (andTid, Prim (And, outer, Var nestedTid), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> (fun value -> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody)) value)
    | Prim (Or, nestedLeft, nestedRight), Let (andTid, Prim (And, Var nestedTid, outer), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> (fun value -> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody)) value)
    | Prim (And, nestedLeft, nestedRight), Let (orTid, Prim (Or, outer, Var nestedTid), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> (fun value -> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody)) value)
    | Prim (And, nestedLeft, nestedRight), Let (orTid, Prim (Or, Var nestedTid, outer), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> (fun value -> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody)) value)
    | Prim (BitOr, nestedLeft, nestedRight), Let (andTid, Prim (BitAnd, outer, Var nestedTid), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> (fun value -> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody)) value)
    | Prim (BitOr, nestedLeft, nestedRight), Let (andTid, Prim (BitAnd, Var nestedTid, outer), andBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> (fun value -> Option.map (fun absorbed -> Let (andTid, Atom absorbed, andBody)) value)
    | Prim (BitAnd, nestedLeft, nestedRight), Let (orTid, Prim (BitOr, outer, Var nestedTid), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> (fun value -> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody)) value)
    | Prim (BitAnd, nestedLeft, nestedRight), Let (orTid, Prim (BitOr, Var nestedTid, outer), orBody)
        when nestedTid = tid ->
        tryAbsorbedAtom outer nestedLeft nestedRight
        |> (fun value -> Option.map (fun absorbed -> Let (orTid, Atom absorbed, orBody)) value)
    | _ -> None

let trySimplifyBoolComplement tid cexpr body =
 let replacementForBoolOp = function And -> Some (BoolLiteral false) | Or -> Some (BoolLiteral true) | _ -> None in
 let simplify source sourceTid boolTid op boolBody = match source with
 | Var originalTid when originalTid=sourceTid -> Option.map (fun replacement -> Let (boolTid,Atom replacement,boolBody)) (replacementForBoolOp op)
 | _ -> None in
 match cexpr,body with
 | UnaryPrim (Not,source),Let (boolTid,Prim (op,Var sourceTid,Var notTid),boolBody) when notTid=tid -> simplify source sourceTid boolTid op boolBody
 | UnaryPrim (Not,source),Let (boolTid,Prim (op,Var notTid,Var sourceTid),boolBody) when notTid=tid -> simplify source sourceTid boolTid op boolBody
 | _ -> None
let trySimplifyInt64BitwiseComplement typeEnv tid cexpr body =
 let replacementForBitwiseOp = function BitAnd -> Some (IntLiteral (Int64 0L)) | BitOr | BitXor -> Some (IntLiteral (Int64 (-1L))) | _ -> None in
 let simplify source sourceTid bitwiseTid op bitwiseBody = match source with
 | Var originalTid when originalTid=sourceTid -> Option.map (fun replacement ->
  let foldedBody = Let (bitwiseTid,Atom replacement,bitwiseBody) in if aExprUsesTemp tid bitwiseBody then Let (tid,cexpr,foldedBody) else foldedBody) (replacementForBitwiseOp op)
 | _ -> None in
 match cexpr,body with
 | UnaryPrim (BitNot,source),Let (bitwiseTid,Prim (op,Var sourceTid,Var notTid),bitwiseBody) when notTid=tid && isInt64Atom typeEnv source -> simplify source sourceTid bitwiseTid op bitwiseBody
 | UnaryPrim (BitNot,source),Let (bitwiseTid,Prim (op,Var notTid,Var sourceTid),bitwiseBody) when notTid=tid && isInt64Atom typeEnv source -> simplify source sourceTid bitwiseTid op bitwiseBody
 | _ -> None
(*
   Optimize an AExpr, returning optimized expression, change flag, and used TempIds
*)
let rec optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv aexpr =
 match tryHoistSharedLeadingBranchBinding context options aexpr with
 | Some replacement -> let replacementResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv replacement in {replacementResult with changed=true}
 | None -> optimizeAExprWithoutBranchHoisting context options env typeEnv tupleEnv cseEnv aexpr
and optimizeAExprWithoutBranchHoisting context options env typeEnv tupleEnv cseEnv aexpr =
 match aexpr with
 | Jump (target,atom) -> let atom' = substAtom env atom in {expr=Jump (target,atom');changed=atom'<>atom;uses=addAtomUse atom' TS.empty}
 | Join (parameter,continuation,entry) ->
  let body = optimizeAExprWithUses context options (TM.remove parameter.id env) (TM.add parameter.id parameter.typ typeEnv) tupleEnv cseEnv continuation in
  let entry' = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv entry in
  {expr=Join (parameter,body.expr,entry'.expr);changed=body.changed || entry'.changed;uses=TS.union (TS.remove parameter.id body.uses) entry'.uses}
 | Return atom -> let atom' = substAtom env atom in {expr=Return atom';changed=atom'<>atom;uses=addAtomUse atom' TS.empty}
 | Let (tid,cexpr,body) ->
  let cexpr',cexprChanged = optimizeCExpr context options env typeEnv tupleEnv cexpr in
  let cexpr'',cseChanged,cseEnv' = if options.enableCSE then
   match tryCSEKey cexpr' with
   | Some key -> (match CSEnv.find_opt key cseEnv with Some existingTid -> Atom (Var existingTid),true,cseEnv | None -> cexpr',false,CSEnv.add key tid cseEnv)
   | None -> cexpr',false,cseEnv
   else cexpr',false,cseEnv in
  let env',skipBinding = match cexpr'' with
  | Atom a when options.enableCopyProp && not (mustPreserveEvaluation context cexpr'') -> TM.add tid a env,true
  | Atom ((IntLiteral _ | BoolLiteral _ | FloatLiteral _ | StringLiteral _ | UnitLiteral) as constAtom) when options.enableConstProp -> TM.add tid constAtom env,false
  | _ -> env,false in
  let tupleEnv' = match cexpr'' with
  | TupleAlloc elements ->
   let forwardableElements = IntMap.of_list (List.filter_map (fun (index,element) -> if canForwardTupleElement context typeEnv element then Some (index,element) else None) (List.mapi (fun index element -> index,element) elements)) in TM.add tid forwardableElements tupleEnv
  | _ -> tupleEnv in
  let bodyResult = optimizeAExprWithUses context options env' typeEnv tupleEnv' cseEnv' body in
  let usesInBody = bodyResult.uses in
  let isDead = options.enableDCE && not (TS.mem tid usesInBody) && not (mustPreserveEvaluation context cexpr'') in
  let usesInBodyWithoutTid = TS.remove tid usesInBody in
  let adjacentSimplification = if options.enableConstFolding then
   (match trySimplifyAdjacentLet context typeEnv tid cexpr'' bodyResult.expr with
   | Some _ as value -> value
   | None -> (match trySimplifyBoolComplement tid cexpr'' bodyResult.expr with Some _ as value -> value | None -> trySimplifyInt64BitwiseComplement typeEnv tid cexpr'' bodyResult.expr)) else None in
  (match adjacentSimplification with
  | Some replacement -> let replacementResult = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv replacement in {replacementResult with changed=true}
  | None when skipBinding -> {expr=bodyResult.expr;changed=true;uses=usesInBodyWithoutTid}
  | _ when isDead -> {expr=bodyResult.expr;changed=true;uses=usesInBodyWithoutTid}
  | _ -> let uses = addCExprUses cexpr'' usesInBodyWithoutTid in {expr=Let (tid,cexpr'',bodyResult.expr);changed=cexprChanged || cseChanged || bodyResult.changed;uses})
 | If (condition,yes,no) ->
  let condition' = substAtom env condition in
  (match condition' with
  | BoolLiteral true when options.enableConstFolding -> let result = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv yes in {expr=result.expr;changed=true;uses=result.uses}
  | BoolLiteral false when options.enableConstFolding -> let result = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv no in {expr=result.expr;changed=true;uses=result.uses}
  | _ ->
   let yes = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv yes in
   let no = optimizeAExprWithUses context options env typeEnv tupleEnv cseEnv no in
   if options.enableConstFolding && yes.expr=Return (BoolLiteral true) && no.expr=Return (BoolLiteral false) then {expr=Return condition';changed=true;uses=addAtomUse condition' TS.empty}
   else if options.enableConstFolding && yes.expr=no.expr then {expr=yes.expr;changed=true;uses=yes.uses}
   else let uses = addAtomUse condition' (TS.union yes.uses no.uses) in {expr=If (condition',yes.expr,no.expr);changed=condition'<>condition || yes.changed || no.changed;uses})
(*
   Optimize an AExpr
*)
let optimizeAExpr context options env typeEnv aexpr = let result = optimizeAExprWithUses context options env typeEnv TM.empty CSEnv.empty aexpr in result.expr,result.changed
(*
   Optimize a function using the stable type metadata for its parameters.
   Initialize env with function parameters (they're not constants)
*)
let optimizeFunction context options typeEnv (func : functionDef) = let body,changed = optimizeAExpr context options TM.empty typeEnv func.body in {func with body},changed
(*
   Optimize until fixed point
*)
let optimizeToFixedPoint context options (func : functionDef) maxIterations =
 let typeEnv = TM.of_list (List.map (fun (param : typedParam) -> param.id,param.typ) func.typedParams) in
 let rec optimize func remaining = if remaining <= 0 then func else let func',changed = optimizeFunction context options typeEnv func in if changed then optimize func' (remaining-1) else func' in
 optimize func maxIterations
let rec maxAExprTempId expr greatest =
 let add (TempId id) greatest = match greatest with None -> Some id | Some previous -> Some (max id previous) in
 let addAtomUse atom greatest = foldAtomTempIds add atom greatest in
 let addCExprUses expr greatest = foldCExprTempIds add expr greatest in
 match expr with
 | Jump (target,atom) -> greatest |> add target |> addAtomUse atom
 | Join (parameter,continuation,entry) -> greatest |> add parameter.id |> maxAExprTempId continuation |> maxAExprTempId entry
 | Return atom -> addAtomUse atom greatest
 | Let (tid,cexpr,body) -> greatest |> add tid |> addCExprUses cexpr |> maxAExprTempId body
 | If (condition,yes,no) -> greatest |> addAtomUse condition |> maxAExprTempId yes |> maxAExprTempId no
(*
   Count uses of a local closure while rejecting every use that is not a call
   through that exact closure value. A positive result proves the allocation
   neither escapes nor reaches storage or an unknown callee.
*)
let rec countKnownClosureCalls closureId expr =
 let addCounts a b = Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b)) in
 let combine left right = match left,right with Some left,Some right -> Some (addCounts left right) | _ -> None in
 let classifyCExpr = function
 | ClosureCall (Var calledId,args) | ClosureTailCall (Var calledId,args) when calledId=closureId && not (atomsUseTemp closureId args) -> Some 1
 | cexpr when not (cexprUsesTemp closureId cexpr) -> Some 0
 | _ -> None in
 match expr with
 | Jump (_,atom) | Return atom -> if atomUsesTemp closureId atom then None else Some 0
 | Let (boundId,cexpr,body) ->
  (match classifyCExpr cexpr with None -> None | Some count when boundId=closureId -> Some count | Some count -> Option.map (addCounts count) (countKnownClosureCalls closureId body))
 | Join (parameter,continuation,entry) ->
  let entry = countKnownClosureCalls closureId entry in let continuation = if parameter.id=closureId then Some 0 else countKnownClosureCalls closureId continuation in combine entry continuation
 | If (condition,yes,no) -> if atomUsesTemp closureId condition then None else let yes = countKnownClosureCalls closureId yes in let no = countKnownClosureCalls closureId no in combine yes no
(*
   Replace proven calls through one capture-free local closure with calls to
   its lifted target. The unused hidden closure argument remains explicit so
   the lifted function's established ABI does not change.
*)
let rec rewriteKnownCaptureFreeCalls closureId funcName expr =
 let rewriteCExpr = function ClosureCall (Var calledId,args) when calledId=closureId -> Call (funcName,UnitLiteral::args) | ClosureTailCall (Var calledId,args) when calledId=closureId -> TailCall (funcName,UnitLiteral::args) | cexpr -> cexpr in
 match expr with
 | Jump _ | Return _ -> expr
 | Let (boundId,cexpr,body) -> let body' = if boundId=closureId then body else rewriteKnownCaptureFreeCalls closureId funcName body in Let (boundId,rewriteCExpr cexpr,body')
 | Join (parameter,continuation,entry) -> let body = if parameter.id=closureId then continuation else rewriteKnownCaptureFreeCalls closureId funcName continuation in Join (parameter,body,rewriteKnownCaptureFreeCalls closureId funcName entry)
 | If (condition,yes,no) -> If (condition,rewriteKnownCaptureFreeCalls closureId funcName yes,rewriteKnownCaptureFreeCalls closureId funcName no)
(*
   Eliminate only capture-free closure allocations whose complete lexical use
   set consists of one or more known calls.
*)
let rec devirtualizeCaptureFreeClosures = function
 | Jump _ | Return _ as expr -> expr
 | Let (closureId,ClosureAlloc (funcName,[]),body) ->
  let body' = devirtualizeCaptureFreeClosures body in
  (match countKnownClosureCalls closureId body' with Some count when count>0 -> rewriteKnownCaptureFreeCalls closureId funcName body' | _ -> Let (closureId,ClosureAlloc (funcName,[]),body'))
 | Let (tid,cexpr,body) -> Let (tid,cexpr,devirtualizeCaptureFreeClosures body)
 | Join (parameter,continuation,entry) -> Join (parameter,devirtualizeCaptureFreeClosures continuation,devirtualizeCaptureFreeClosures entry)
 | If (condition,yes,no) -> If (condition,devirtualizeCaptureFreeClosures yes,devirtualizeCaptureFreeClosures no)
let freshVarGenForProgram (Program (functions,main)) =
 let greatest = List.fold_left (fun greatest (func : functionDef) ->
  let greatest = List.fold_left (fun greatest (parameter : typedParam) -> let TempId id = parameter.id in match greatest with None -> Some id | Some previous -> Some (max id previous)) greatest func.typedParams in
  maxAExprTempId func.body greatest) None functions |> maxAExprTempId main in
 match greatest with None -> initialVarGen | Some id -> VarGen (Int32.to_int (Int32.add (Int32.of_int id) 1l))
