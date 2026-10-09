(* Complete A-normal form data, frozen type tables, and coverage identities. *)
[@@@warning "-30"]

type tempId = TempId of int
type typedParam = { id : tempId; typ : AST.semanticType }

type sizedInt =
  | Int8 of int
  | Int16 of int
  | Int32 of int32
  | Int64 of int64
  | UInt8 of int
  | UInt16 of int
  | UInt32 of int64
  | UInt64 of int64

val sizedIntToInt64 : sizedInt -> int64
val sizedIntToString : sizedInt -> string
val sizedIntToType : sizedInt -> AST.semanticType

type atom =
  | UnitLiteral
  | IntLiteral of sizedInt
  | BoolLiteral of bool
  | StringLiteral of string
  | FloatLiteral of float
  | Var of tempId
  | FuncRef of AST.functionId

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

type unaryOp = Neg | Not | BitNot
type returnOwnership = OwnedReturn | BorrowedReturn

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
 | SocketSendTo
  | SocketReceive
 | SocketReceiveFrom
  | SocketReceiveTimeout
  | SocketSendTimeout
  | SocketClose
  | SocketBind4
 | SocketBind6
  | SocketListen
  | SocketAccept
  | SocketCloexec
  | SocketReuseAddress
  | SocketPoll
  | SignalBlock
  | SignalRestore
  | SignalPending
  | SignalWait
  | MonotonicTime
  | SecureRandomFill
  | PosixOpenAt
  | PosixRead
  | PosixWrite
  | PosixClose
  | PosixSeek
  | PosixStatAt
  | PosixGetCwd
  | PosixChdir
  | PosixMkdirAt
  | PosixUnlinkAt
  | PosixRenameAt
  | PosixChmodAt
  | PosixChmodAt2
  | PosixUtimesAt
  | PosixSetAttributesAt
  | PosixSymlinkAt
  | PosixReadlinkAt
  | PosixFlock
  | PosixGetDents
  | PosixIoctl

type recordDescriptor = {
  sourceTypeName : string;
  runtimeTypeName : string;
  typeArgs : AST.semanticType list;
  fields : (string * AST.semanticType) list;
  valueType : AST.semanticType;
}

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
  | RefCountInc of
      atom * int * MemoryModel.rcKind * MemoryModel.rcMetadata option
  | RefCountDec of
      atom * int * MemoryModel.rcKind * MemoryModel.rcMetadata option
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

type aExpr =
  | Let of tempId * cExpr * aExpr
  | Return of atom
  | If of atom * aExpr * aExpr
  | Join of typedParam * aExpr * aExpr
  | Jump of tempId * atom

type functionDef = {
  id : AST.functionId;
  name : string;
  typedParams : typedParam list;
  returnType : AST.semanticType;
  returnOwnership : returnOwnership;
  body : aExpr;
}

type program = Program of functionDef list * aExpr
type varGen = VarGen of int

val freshVar : varGen -> tempId * varGen
val initialVarGen : varGen

type typeMap

module TypeMap : sig
  val empty : typeMap
  val tryFind : tempId -> typeMap -> AST.semanticType option
  val ofSeq : (tempId * AST.semanticType) Seq.t -> typeMap
  val merge : typeMap -> typeMap -> typeMap
end

type typedProgram = { program : program; typeMap : typeMap }
type exprId = int
type exprIdGen = ExprIdGen of int

val freshExprId : exprIdGen -> exprId * exprIdGen
val initialExprIdGen : exprIdGen

module ExprIdMap : Map.S with type key = int

type coverageMapping = {
  descriptions : string ExprIdMap.t;
  totalExpressions : int;
}

val emptyCoverageMapping : coverageMapping
val addCoverageEntry : exprId -> string -> coverageMapping -> coverageMapping
