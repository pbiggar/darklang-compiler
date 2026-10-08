(* MIR.mli - Mid-level Intermediate Representation. *)
[@@@warning "-4-30"]

type vReg = VReg of int

module VRegMap : Map.S with type key = vReg
module VRegSet : Set.S with type elt = vReg
module IntSet : Set.S with type elt = int

type typedMIRParam = { reg : vReg; typ : AST.semanticType }

type operand =
  | Int64Const of int64
  | BoolConst of bool
  | FloatSymbol of float
  | StringSymbol of string
  | Register of vReg
  | FuncAddr of AST.functionId

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
type rcKind = GenericHeap | StreamHeap | TaggedList | DictHeap | ClosureHeap

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
  | SocketBind4
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

type label = Label of string

module LabelMap : Map.S with type key = label
module LabelSet : Set.S with type elt = label

type instr =
  | Mov of vReg * operand * AST.semanticType option
  | BinOp of vReg * binOp * operand * operand * AST.semanticType
  | UnaryOp of vReg * unaryOp * operand
  | Call of
      vReg
      * AST.functionId
      * operand list
      * AST.semanticType list
      * AST.semanticType
  | TailCall of
      AST.functionId * operand list * AST.semanticType list * AST.semanticType
  | IndirectCall of
      vReg * operand * operand list * AST.semanticType list * AST.semanticType
  | IndirectTailCall of
      operand * operand list * AST.semanticType list * AST.semanticType
  | ClosureAlloc of vReg * AST.functionId * operand list
  | ClosureCall of
      vReg * operand * operand list * AST.semanticType list * AST.semanticType
  | ClosureTailCall of operand * operand list * AST.semanticType list
  | HeapAlloc of vReg * int
  | HeapStore of vReg * int * operand * AST.semanticType option
  | HeapLoad of vReg * vReg * int * AST.semanticType option
  | StringConcat of vReg * operand * operand * operand list
  | CanonicalBufferEq of
      vReg * MemoryModel.canonicalBufferKind * operand * operand
  | RefCountInc of vReg * int * rcKind * MemoryModel.rcMetadata option
  | RefCountDec of vReg * int * rcKind * MemoryModel.rcMetadata option
  | Print of operand * AST.semanticType
  | StdoutWrite of int * operand * bool
  | StdinReadLine of vReg
  | RuntimeError of string
  | RuntimeErrorString of operand
  | FileReadBlob of vReg * operand
  | FileExists of vReg * operand
  | FileWriteBlob of vReg * operand * operand
  | FileAppendText of vReg * operand * operand
  | FileDelete of vReg * operand
  | FileCreateDirectory of vReg * operand
  | FileSetExecutable of vReg * operand
  | FileWriteFromPtr of vReg * operand * operand * operand
  | FloatSqrt of vReg * operand
  | FloatAbs of vReg * operand
  | FloatNeg of vReg * operand
  | Int64ToFloat of vReg * operand
  | FloatToInt64 of vReg * operand
  | FloatToBits of vReg * operand
  | RawAlloc of vReg * operand
  | MappedAlloc of vReg * operand
  | RawFree of operand
  | MappedFree of operand
  | RawGet of vReg * operand * operand * AST.semanticType option
  | RawGetByte of vReg * operand * operand
  | RawWriteWord of operand * operand * operand
  | RawWriteByte of operand * operand * operand
  | RawSlotInit of operand * operand * operand * AST.semanticType
  | StringToRawPtr of vReg * operand
  | RawPtrToString of vReg * operand
  | BlobToRawPtr of vReg * operand
  | RawPtrToBlob of vReg * operand
  | DictToRawPtr of vReg * operand
  | RawPtrToDict of vReg * operand * operand
  | ListToRawPtr of vReg * operand
  | RawPtrToList of vReg * operand * operand
  | RefCountIncString of operand
  | RefCountDecString of operand
  | RefCountIncBlob of operand
  | RefCountDecBlob of operand
  | RefCountIncInt of operand
  | RefCountDecInt of operand
  | RandomInt64 of vReg
  | DateTimeNow of vReg
  | Sleep of int * vReg * operand
  | CliNative of vReg * cliOperation * operand list
  | FloatToString of vReg * operand
  | Phi of vReg * (operand * label) list * AST.semanticType option
  | CoverageHit of int

type terminator =
  | Ret of operand
  | Branch of operand * label * label
  | Jump of label

type basicBlock = {
  label : label;
  instrs : instr list;
  terminator : terminator;
}

type cfg = { entry : label; blocks : basicBlock LabelMap.t }

type functionDef = {
  id : AST.functionId;
  name : string;
  typedParams : typedMIRParam list;
  returnType : AST.semanticType;
  cfg : cfg;
  floatRegs : IntSet.t;
}

type variantInfo = {
  name : string;
  tag : int;
  payload : AST.semanticType option;
  fieldCount : int;
}

type typeVariants = { typeParams : string list; variants : variantInfo list }
type variantRegistry = typeVariants StringOrder.Map.t
type recordField = { name : string; typ : AST.semanticType }
type recordRegistry = recordField list StringOrder.Map.t
type program = Program of functionDef list * variantRegistry * recordRegistry
type regGen = RegGen of int
type labelGen = LabelGen of int

val freshReg : regGen -> vReg * regGen
val freshLabelWithPrefix : string -> labelGen -> label * labelGen
val initialLabelGen : labelGen
