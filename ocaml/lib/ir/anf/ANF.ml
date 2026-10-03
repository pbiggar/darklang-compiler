(*
   ANF.fs - A-Normal Form Intermediate Representation
   Defines the ANF (A-Normal Form) data structures.
   ANF is an intermediate representation where:
   - All intermediate computations are named with temporary variables
   - All operands to operations are simple (variables or literals, called "atoms")
   - Evaluation order is completely explicit through let-bindings
   This representation simplifies subsequent compiler passes by eliminating
   nested expressions.
   Example ANF for "2 + 3 * 4":
   let tmp0 = 3
   let tmp1 = 4
   let tmp2 = tmp0 * tmp1
   let tmp3 = 2
   let tmp4 = tmp3 + tmp2
   return tmp4
   ANF function definition
   Parameter IDs with their types bundled
*)
(* Complete A-normal form data, frozen type tables, and coverage identities. *)
[@@@warning "-30"]
(*
   Unique identifier for temporary variables
*)
type tempId = TempId of int
(*
   Parameter with type information bundled together (makes invalid states unrepresentable)
*)
type typedParam = {id : tempId; typ : AST.semanticType}
(*
   Integer value with explicit size - invalid states unrepresentable
   Following "make invalid states unrepresentable" principle
*)
type sizedInt = Int8 of int | Int16 of int | Int32 of int32 | Int64 of int64 | UInt8 of int | UInt16 of int | UInt32 of int64 | UInt64 of int64
(*
   Atomic expressions (cannot be decomposed further)
   Unit value: ()
   Integer with explicit size
*)
type atom =
 | UnitLiteral
 | IntLiteral of sizedInt
 | BoolLiteral of bool
 | StringLiteral of string
 | FloatLiteral of float
 | Var of tempId
 | FuncRef of AST.functionId
(*
   Binary operations on atoms
   Arithmetic
   Bitwise
   << (left shift)
   >> (right shift)
   & (bitwise and)
   ||| (bitwise or)
   ^ (bitwise xor)
   Comparisons
   Boolean
*)
type binOp =
 | Add
 | Sub
 | Mul
 | Div
 | Mod
 | Shl
 | Shr
 | BitAnd
 | BitOr
 | BitXor
 | Eq
 | Neq
 | Lt
 | Gt
 | Lte
 | Gte
 | And
 | Or
(*
   Unary operations on atoms
   Bitwise NOT: ~~~expr
*)
type unaryOp =
 | Neg
 | Not
 | BitNot
(*
   Function return ownership convention
*)
type returnOwnership =
 | OwnedReturn
 | BorrowedReturn
(*
   Typed native effects retained after the portable Stdlib.Cli wrappers lower.
*)
type cliOperation =
 | Execute
 | RunProcess
 | HostOS
 | HostArchitecture
 | Hostname
 | GetEnv
 | GetEnvironmentPacked
 | SetEnv
 | UnsetEnv
 | DirectoryCurrent
 | DirectoryListPacked
 | FileIsDirectory
 | FileCreateExclusive
 | GetArgv
 | Kill
 | GetPid
 | GetUid
 | CpuCount
 | SpawnProcess
 | ProcessIO
 | TerminateProcess
 | SocketTcp4
 | SocketTcp6
 | SocketUdp4
 | SocketUdp6
 | SocketConnect4
 | SocketConnect6
 | SocketSend
 | SocketReceive
 | SocketReceiveTimeout
 | SocketSendTimeout
 | SocketClose
 | SecureRandomFill
(*
   Immutable nominal metadata carried through fixed-block lowering for field
   layout, ownership, diagnostics, and rendering. It is compile-time metadata,
   not a native field.
   The nominal value represented by this fixed block. Constructor lowering
   also uses this descriptor for boxed sums whose physical layout is
   [tag, payload].
*)
type recordDescriptor = {sourceTypeName : string; runtimeTypeName : string; typeArgs : AST.semanticType list; fields : (string * AST.semanticType) list; valueType : AST.semanticType}
(*
   Complex expressions (produce values)
   Atom with explicit type (for pattern matching where inferred types would be wrong)
   If-expression that produces a value
   Call through function variable (BLR instruction)
   Tail call through function variable (BR instruction)
   Call through closure, passing closure as hidden first arg
   Tail call through closure (BR instruction)
   Create tuple: (a, b, c)
   Get tuple element: t.0
   String operations (heap-allocating)
   Concatenate at least two strings with one allocation and ordered copies.
   Reference counting operations
   Increment ref count of heap value
   Decrement ref count, free if zero
   Output operations (for main expression result)
   Print value with type-appropriate formatting
   Explicit stdout effect; returns Unit
   Read one UTF-8 line from stdin; returns String
   Print runtime error to stderr and exit with code 1
   Print a language String error to stderr and exit with code 1
   File I/O intrinsics (generate syscalls)
   Read file, returns Result<Blob, String>
   Check if file exists, returns Bool
   Write Blob, returns Result<Unit, String>
   Append to file, returns Result<Unit, String>
   Delete file, returns Result<Unit, String>
   Create directory, returns Result<Unit, String>
   Set executable bit, returns Result<Unit, String>
   Write raw bytes from pointer to file
   Float intrinsics
   Square root: sqrt(x)
   Absolute value: |x|
   Negate: -x
   Convert Int64 to Float64
   Convert Float64 to Int64 (truncate)
   Copy Float64 bits to UInt64
   Raw memory intrinsics (internal, for HAMT implementation)
   Allocate raw bytes (no header), returns RawPtr
   Independent mapping, private size prefix, explicit lifetime
   Manually free raw memory
   Unmap exactly one MappedAlloc payload; never a heap pointer
   Read 8 bytes at offset, valueType for float
   Transfer a typed slot edge to the result
   Read 1 byte at offset, returns Int64 (zero-extended)
   Write 8 bytes without retaining; RC insertion also uses this for transferred typed edges
   Write 1 unmanaged byte at offset
   Initialize typed 8-byte slot edge at offset
   Borrow raw backing pointer from String
   Reinterpret raw allocation as owned String
   Borrow raw backing pointer from Blob
   Reinterpret raw allocation as owned Blob
   Adopt an initialized fixed Int128 block
   Adopt an initialized fixed UInt128 block
   Strip Dict tag bits, returning RawPtr
   Re-tag RawPtr as Dict
   Strip List tag bits, returning RawPtr
   Borrow an untagged fixed-block payload pointer
   Re-tag RawPtr as List
   Dynamic buffer reference counting at the value pointer
   Increment string ref count
   Decrement string ref count, free if zero
   Increment bytes ref count
   Decrement bytes ref count, free if zero
   Increment heap-backed Int ref count
   Decrement heap-backed Int ref count
   Random intrinsics
   Get 8 random bytes as Int64
   DateTime intrinsics
   Get the current UTC instant as 100ns Unix ticks
   Blocking typed native delay in milliseconds
   Float to String conversion
   Convert Float to heap String
*)
type cExpr =
 | Atom of atom
 | TypedAtom of atom * AST.semanticType
 | Prim of binOp * atom * atom
 | UnaryPrim of unaryOp * atom
 | IfValue of atom * atom * atom
 | Call of AST.functionId * atom list
 | BorrowedCall of AST.functionId * atom list
 | TailCall of AST.functionId * atom list
 | IndirectCall of atom * atom list
 | IndirectTailCall of atom * atom list
 | ClosureAlloc of AST.functionId * atom list
 | ClosureCall of atom * atom list
 | ClosureTailCall of atom * atom list
 | TupleAlloc of atom list
 | TupleGet of atom * int
 | RecordAlloc of recordDescriptor * atom list
 | RecordGet of recordDescriptor * atom * int
 | RecordClone of recordDescriptor * atom * atom list
 | RecordReuse of recordDescriptor * recordDescriptor * atom * atom list
 | StringConcat of atom * atom * atom list
 | CanonicalBufferEq of MemoryModel.canonicalBufferKind * atom * atom
 | RefCountInc of atom * int * MemoryModel.rcKind * MemoryModel.rcMetadata option
 | RefCountDec of atom * int * MemoryModel.rcKind * MemoryModel.rcMetadata option
 | Print of atom * AST.semanticType
 | StdoutWrite of atom * bool
 | StdinReadLine
 | RuntimeError of string
 | RuntimeErrorString of atom
 | FileReadBlob of atom
 | FileExists of atom
 | FileWriteBlob of atom * atom
 | FileAppendText of atom * atom
 | FileDelete of atom
 | FileCreateDirectory of atom
 | FileSetExecutable of atom
 | FileWriteFromPtr of atom * atom * atom
 | FloatSqrt of atom
 | FloatAbs of atom
 | FloatNeg of atom
 | Int64ToFloat of atom
 | FloatToInt64 of atom
 | FloatToBits of atom
 | RawAlloc of atom
 | MappedAlloc of atom
 | RawFree of atom
 | MappedFree of atom
 | RawGet of atom * atom * AST.semanticType option
 | RawTake of atom * atom * AST.semanticType option
 | RawGetByte of atom * atom
 | RawWriteWord of atom * atom * atom
 | RawWriteByte of atom * atom * atom
 | RawSlotInit of atom * atom * atom * AST.semanticType
 | StringToRawPtr of atom
 | RawPtrToString of atom
 | BlobToRawPtr of atom
 | RawPtrToBlob of atom
 | RawPtrToInt128 of atom
 | RawPtrToUInt128 of atom
 | DictToRawPtr of atom
 | RawPtrToDict of atom * atom * AST.semanticType
 | ListToRawPtr of atom
 | FixedBlockToRawPtr of atom
 | RawPtrToList of atom * atom * AST.semanticType
 | RefCountIncString of atom
 | RefCountDecString of atom
 | RefCountIncBlob of atom
 | RefCountDecBlob of atom
 | RefCountIncInt of atom
 | RefCountDecInt of atom
 | RandomInt64
 | DateTimeNow
 | Sleep of atom
 | CliNative of cliOperation * atom list
 | FloatToString of atom
(*
   ANF expressions with explicit sequencing
   Nonrecursive lexical continuation with one immediate block argument.
   The parameter identity names the target in entry and binds its value
   only in continuation. The continuation may target enclosing joins.
*)
type aExpr =
 | Let of tempId * cExpr * aExpr
 | Return of atom
 | If of atom * aExpr * aExpr
 | Join of typedParam * aExpr * aExpr
 | Jump of tempId * atom
type functionDef = {id : AST.functionId; name : string; typedParams : typedParam list; returnType : AST.semanticType; returnOwnership : returnOwnership; body : aExpr}
(*
   ANF program (functions and main expression)
*)
type program = Program of functionDef list * aExpr
(*
   Fresh variable generator (functional style)
*)
type varGen = VarGen of int
(*
   Frozen program-wide type information. The offset avoids allocating the
   unused prefix of independently lowered units; gaps denote absent values.
   Duplicate IDs retain the last definition, matching accumulation order.
   Sorting once permits filling gaps between allocated IDs.
   Overlay later metadata while preserving entries absent from that unit.
*)
type typeMap = {firstId : int; types : AST.semanticType option array}
let int32Add left right = Int32.to_int (Int32.add (Int32.of_int left) (Int32.of_int right))
let int32Sub left right = Int32.to_int (Int32.sub (Int32.of_int left) (Int32.of_int right))
module TypeMap = struct
 module IdMap = Map.Make(Int)
 let empty = {firstId = 0; types = [||]}
 let tryFind (TempId id) types =
  let index = int32Sub id types.firstId in
  if index < 0 || index >= Array.length types.types then None else types.types.(index)
 let ofSeq entries =
  let ordered = Seq.fold_left (fun map (TempId id, typ) -> IdMap.add id typ map) IdMap.empty entries in
  match IdMap.min_binding_opt ordered with
  | None -> empty
  | Some (first, _) ->
   if first < 0 then Crash.crash "ANF type table contains a negative TempId";
   let last = fst (IdMap.max_binding ordered) in
   let types = Array.init (last - first + 1) (fun index -> IdMap.find_opt (first + index) ordered) in
   {firstId = first; types}
 let merge earlier later =
  if Array.length earlier.types = 0 then later else if Array.length later.types = 0 then earlier else
  let first = min earlier.firstId later.firstId in
  let last = max (int32Add earlier.firstId (Array.length earlier.types)) (int32Add later.firstId (Array.length later.types)) in
  {firstId = first; types = Array.init (int32Sub last first) (fun index -> let id = TempId (int32Add first index) in match tryFind id later with Some _ as value -> value | None -> tryFind id earlier)}
end
(*
   Program with type information for reference counting
*)
type typedProgram = {program : program; typeMap : typeMap}
(*
   Coverage Types
   Unique expression ID for coverage tracking
*)
type exprId = int
(*
   Expression ID generator (functional style, like VarGen)
*)
type exprIdGen = ExprIdGen of int
module ExprIdMap = Map.Make(Int)
(*
   Coverage mapping: tracks expression descriptions for reporting
   ExprId -> description string (e.g., "List.map: Call filter")
   Total number of expressions tracked
*)
type coverageMapping = {descriptions : string ExprIdMap.t; totalExpressions : int}
(*
   Convert a SizedInt to the signed 64-bit payload used by MIR integer constants.
   UInt64 values above Int64.MaxValue intentionally wrap to the same 64 bits.
*)
let sizedIntToInt64 = function
 | Int8 value | Int16 value | UInt8 value | UInt16 value -> Int64.of_int value
 | Int32 value -> Int64.of_int32 value
 | Int64 value | UInt32 value | UInt64 value -> value
(*
   Format a SizedInt as the corresponding source-level integer value.
*)
let sizedIntToString = function
 | Int8 value | Int16 value | UInt8 value | UInt16 value -> string_of_int value
 | Int32 value -> Int32.to_string value
 | Int64 value | UInt32 value -> Int64.to_string value
 | UInt64 value -> Z.to_string (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64) else Z.of_int64 value)
(*
   Get the AST.SemanticType corresponding to a SizedInt
*)
let sizedIntToType = function Int8 _ -> AST.TInt8 | Int16 _ -> AST.TInt16 | Int32 _ -> AST.TInt32 | Int64 _ -> AST.TInt64 | UInt8 _ -> AST.TUInt8 | UInt16 _ -> AST.TUInt16 | UInt32 _ -> AST.TUInt32 | UInt64 _ -> AST.TUInt64
(*
   Generate a fresh temporary variable
*)
let freshVar (VarGen n) = TempId n, VarGen (int32Add n 1)
(*
   Initial variable generator
*)
let initialVarGen = VarGen 0
(*
   Generate a fresh expression ID
*)
let freshExprId (ExprIdGen n) = n, ExprIdGen (int32Add n 1)
(*
   Initial expression ID generator
*)
let initialExprIdGen = ExprIdGen 0
(*
   Empty coverage mapping
*)
let emptyCoverageMapping = {descriptions = ExprIdMap.empty; totalExpressions = 0}
(*
   Add an expression to the coverage mapping
*)
let addCoverageEntry exprId description mapping = {descriptions = ExprIdMap.add exprId description mapping.descriptions; totalExpressions = max mapping.totalExpressions (int32Add exprId 1)}
