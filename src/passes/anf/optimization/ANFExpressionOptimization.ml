(* ANFExpressionOptimization.ml - Value-numbering keys and fresh temporary allocation shared by SSA and ANF lowering. *)
[@@@warning "-4"]

open ANF
open ANFConstants
open ANFEffects
module TM = TempMap


type scalarUnaryCSEOp =
  | PrimitiveUnary of unaryOp
  | FloatSqrtOp
  | FloatAbsOp
  | FloatNegOp
  | Int64ToFloatOp
  | FloatToInt64Op
  | FloatToBitsOp

type cSEKey =
  | BinaryValue of binOp * atom * atom
  | UnaryValue of scalarUnaryCSEOp * atom
  | ConditionalValue of atom * atom * atom
  | TupleProjection of atom * int
  | RecordProjection of recordDescriptor * atom * int

let compareSizedInt a b =
  let rank = function
    | Int8 _ -> 0
    | Int16 _ -> 1
    | Int32 _ -> 2
    | Int64 _ -> 3
    | UInt8 _ -> 4
    | UInt16 _ -> 5
    | UInt32 _ -> 6
    | UInt64 _ -> 7
  in
  let rankOrder = Int.compare (rank a) (rank b) in
  if rankOrder <> 0 then rankOrder
  else
    match (a, b) with
    | Int8 a, Int8 b | Int16 a, Int16 b | UInt8 a, UInt8 b | UInt16 a, UInt16 b
      ->
        Int.compare a b
    | Int32 a, Int32 b -> Int32.compare a b
    | Int64 a, Int64 b | UInt32 a, UInt32 b -> Int64.compare a b
    | UInt64 a, UInt64 b -> Int64.unsigned_compare a b
    | _ ->
        Crash.crash
          "ANFExpressionOptimization: equal integer ranks have different \
           constructors"

let compareAtom a b =
  let rank = function
    | UnitLiteral -> 0
    | IntLiteral _ -> 1
    | BoolLiteral _ -> 2
    | StringLiteral _ -> 3
    | FloatLiteral _ -> 4
    | Var _ -> 5
    | FuncRef _ -> 6
  in
  let rankOrder = Int.compare (rank a) (rank b) in
  if rankOrder <> 0 then rankOrder
  else
    match (a, b) with
    | UnitLiteral, UnitLiteral -> 0
    | IntLiteral a, IntLiteral b -> compareSizedInt a b
    | BoolLiteral a, BoolLiteral b -> Bool.compare a b
    | StringLiteral a, StringLiteral b -> StringOrder.compare a b
    | FloatLiteral a, FloatLiteral b ->
        (* Value-numbering keys preserve representations observable by Float.toBits. *)
        Int64.compare (Int64.bits_of_float a) (Int64.bits_of_float b)
    | Var (TempId a), Var (TempId b) -> Int.compare a b
    | FuncRef a, FuncRef b ->
        Int64.unsigned_compare (AST.functionIdValue a) (AST.functionIdValue b)
    | _ ->
        Crash.crash
          "ANFExpressionOptimization: equal atom ranks have different \
           constructors"

let rec compareList cmp xs ys =
  match (xs, ys) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
      let order = cmp x y in
      if order = 0 then compareList cmp xs ys else order

let thenCompare first next = if first = 0 then next () else first

let compareDescriptor a b =
  thenCompare (StringOrder.compare a.sourceTypeName b.sourceTypeName) (fun () ->
      thenCompare (StringOrder.compare a.runtimeTypeName b.runtimeTypeName)
        (fun () ->
          thenCompare
            (compareList AST.compareSemanticType a.typeArgs b.typeArgs)
            (fun () ->
              thenCompare
                (compareList
                   (fun (an, at) (bn, bt) ->
                     thenCompare (StringOrder.compare an bn) (fun () ->
                         AST.compareSemanticType at bt))
                   a.fields b.fields)
                (fun () -> AST.compareSemanticType a.valueType b.valueType))))

let compareUnary a b =
  let rank = function
    | PrimitiveUnary _ -> 0
    | FloatSqrtOp -> 1
    | FloatAbsOp -> 2
    | FloatNegOp -> 3
    | Int64ToFloatOp -> 4
    | FloatToInt64Op -> 5
    | FloatToBitsOp -> 6
  in
  thenCompare
    (Int.compare (rank a) (rank b))
    (fun () ->
      match (a, b) with
      | PrimitiveUnary a, PrimitiveUnary b -> Stdlib.compare a b
      | _ -> 0)

let compareKey a b =
  let rank = function
    | BinaryValue _ -> 0
    | UnaryValue _ -> 1
    | ConditionalValue _ -> 2
    | TupleProjection _ -> 3
    | RecordProjection _ -> 4
  in
  thenCompare
    (Int.compare (rank a) (rank b))
    (fun () ->
      match (a, b) with
      | BinaryValue (op, a, b), BinaryValue (op', a', b') ->
          thenCompare (Stdlib.compare op op') (fun () ->
              thenCompare (compareAtom a a') (fun () -> compareAtom b b'))
      | UnaryValue (op, a), UnaryValue (op', a') ->
          thenCompare (compareUnary op op') (fun () -> compareAtom a a')
      | ConditionalValue (a, b, c), ConditionalValue (a', b', c') ->
          thenCompare (compareAtom a a') (fun () ->
              thenCompare (compareAtom b b') (fun () -> compareAtom c c'))
      | TupleProjection (a, index), TupleProjection (b, index') ->
          thenCompare (compareAtom a b) (fun () -> Int.compare index index')
      | RecordProjection (d, a, index), RecordProjection (d', b, index') ->
          thenCompare (compareDescriptor d d') (fun () ->
              thenCompare (compareAtom a b) (fun () -> Int.compare index index'))
      | _ ->
          Crash.crash
            "ANFExpressionOptimization: equal CSE key ranks have different \
             constructors")

module CSEnv = Map.Make (struct
  type t = cSEKey

  let compare = compareKey
end)

let isCommutativeBinOp = function
  | Add | Mul | Eq | Neq | And | Or | BitAnd | BitOr | BitXor -> true
  | Sub | Div | Mod | Lt | Gt | Lte | Gte | Shl | Shr -> false

(*
   Canonicalize relational comparisons to their less-than spelling so
   reversing both the operator and operands produces the same CSE key.
*)
let binaryCSEKey op left right =
  match op with
  | Gt -> BinaryValue (Lt, right, left)
  | Gte -> BinaryValue (Lte, right, left)
  | _ when isCommutativeBinOp op && compareAtom right left < 0 ->
      BinaryValue (op, right, left)
  | _ -> BinaryValue (op, left, right)

let isRecordProjectionCSEType = function
  | AST.TInt64 | AST.TInt32 | AST.TInt16 | AST.TInt8 | AST.TUInt64 | AST.TUInt32
  | AST.TUInt16 | AST.TUInt8 | AST.TBool | AST.TChar | AST.TDateTime ->
      true
  | AST.TInt128 | AST.TInt | AST.TUInt128 | AST.TFloat64 | AST.TString
  | AST.TBlob | AST.TUnit | AST.TNever | AST.TFunction _ | AST.TTuple _
  | AST.TRecord _ | AST.TSum _ | AST.TList _ | AST.TStream _ | AST.TVar _
  | AST.TInferenceVar _ | AST.TInternalRawPtr | AST.TDict _ ->
      false

(*
   Return a value-numbering key only when merging two evaluations preserves
   allocation identity, mutable-memory observations, and ownership semantics.
   Keep this match exhaustive so every new CExpr case requires an explicit CSE
   decision instead of silently falling through a permissive purity test.
*)
let tryCSEKey = function
  | Prim (op, left, right) -> Some (binaryCSEKey op left right)
  | UnaryPrim (op, atom) -> Some (UnaryValue (PrimitiveUnary op, atom))
  | IfValue (condition, yes, no) -> Some (ConditionalValue (condition, yes, no))
  | FloatSqrt atom -> Some (UnaryValue (FloatSqrtOp, atom))
  | FloatAbs atom -> Some (UnaryValue (FloatAbsOp, atom))
  | FloatNeg atom -> Some (UnaryValue (FloatNegOp, atom))
  | Int64ToFloat atom -> Some (UnaryValue (Int64ToFloatOp, atom))
  | FloatToInt64 atom -> Some (UnaryValue (FloatToInt64Op, atom))
  | FloatToBits atom -> Some (UnaryValue (FloatToBitsOp, atom))
  | TupleGet (tuple, index) -> Some (TupleProjection (tuple, index))
  | RecordGet (descriptor, record, index) -> (
      match
        if index < 0 then None else List.nth_opt descriptor.fields index
      with
      | Some (_, typ) when isRecordProjectionCSEType typ ->
          Some (RecordProjection (descriptor, record, index))
      | Some _ -> None
      | None ->
          Crash.crash
            ("ANF CSE: invalid field index " ^ string_of_int index
           ^ " for record " ^ descriptor.runtimeTypeName))
  | Atom _ | TypedAtom _ | Call _ | BorrowedCall _ | TailCall _ | IndirectCall _
  | IndirectTailCall _ | ClosureAlloc _ | ClosureCall _ | ClosureTailCall _
  | TupleAlloc _ | RecordAlloc _ | RecordClone _ | RecordReuse _
  | StringConcat _ | CanonicalBufferEq _ | RefCountInc _ | RefCountDec _
  | Print _ | StdoutWrite _ | StdinReadLine | RuntimeError _
  | RuntimeErrorString _ | FileReadBlob _ | FileExists _ | FileWriteBlob _
  | FileAppendText _ | FileDelete _ | FileCreateDirectory _
  | FileSetExecutable _ | FileWriteFromPtr _ | RawAlloc _ | MappedAlloc _
  | RawFree _ | RawFreeSized _ | MappedFree _ | RawGet _ | RawTake _
  | RawGetByte _ | RawWriteWord _ | RawWriteByte _ | RawSlotInit _
  | StringToRawPtr _ | RawPtrToString _ | BlobToRawPtr _ | RawPtrToBlob _
  | RawPtrToInt128 _ | RawPtrToUInt128 _ | DictToRawPtr _ | RawPtrToDict _
  | ListToRawPtr _ | FixedBlockToRawPtr _ | RawPtrToList _ | RefCountIncString _
  | RefCountDecString _ | RefCountIncBlob _ | RefCountDecBlob _
  | RefCountIncInt _ | RefCountDecInt _ | RandomInt64 | DateTimeNow | Sleep _
  | CliNative _ | FloatToString _ ->
      None

let rec maxAExprTempId expr greatest =
  let add (TempId id) greatest =
    match greatest with
    | None -> Some id
    | Some previous -> Some (max id previous)
  in
  let addAtomUse atom greatest = foldAtomTempIds add atom greatest in
  let addCExprUses expr greatest = foldCExprTempIds add expr greatest in
  match expr with
  | Jump (target, atom) -> greatest |> add target |> addAtomUse atom
  | Join (parameter, continuation, entry) ->
      greatest |> add parameter.id
      |> maxAExprTempId continuation
      |> maxAExprTempId entry
  | Return atom -> addAtomUse atom greatest
  | Let (tid, cexpr, body) ->
      greatest |> add tid |> addCExprUses cexpr |> maxAExprTempId body
  | If (condition, yes, no) ->
      greatest |> addAtomUse condition |> maxAExprTempId yes
      |> maxAExprTempId no

let freshVarGenForProgram (Program (functions, main)) =
  let greatest =
    List.fold_left
      (fun greatest (func : functionDef) ->
        let greatest =
          List.fold_left
            (fun greatest (parameter : typedParam) ->
              let (TempId id) = parameter.id in
              match greatest with
              | None -> Some id
              | Some previous -> Some (max id previous))
            greatest func.typedParams
        in
        maxAExprTempId func.body greatest)
      None functions
    |> maxAExprTempId main
  in
  match greatest with
  | None -> initialVarGen
  | Some id -> VarGen (Int32.to_int (Int32.add (Int32.of_int id) 1l))
