(*
   Defines the MIR (Mid-level IR) data structures.
   MIR is a platform-independent three-address code representation where:
   - Each instruction has at most two operands and one destination
   - Virtual registers are used (infinite supply)
   - Instructions are organized into basic blocks
   Example MIR:
   v0 <- 2
   v1 <- 3
   v2 <- v0 + v1
   ret v2
   Control Flow Graph
   MIR function with CFG
   Parameters with types bundled
   Return type (for distinguishing int vs float returns)
   VReg IDs that hold float values (for SSA phi nodes)
   MIR program (list of functions plus type/record registries)
*)
(* MIR.ml - Mid-level Intermediate Representation. *)
[@@@warning "-4-30"]

(*
   Virtual register (infinite supply)
*)
type vReg = VReg of int

module VRegMap = Map.Make (struct
  type t = vReg

  let compare (VReg left) (VReg right) = Int.compare left right
end)

module VRegSet = Set.Make (struct
  type t = vReg

  let compare (VReg left) (VReg right) = Int.compare left right
end)

module IntSet = Set.Make (Int)

(*
   Parameter with register and type bundled (makes invalid states unrepresentable)
*)
type typedMIRParam = { reg : vReg; typ : AST.semanticType }

(*
   Operands
   Address of a function (for higher-order functions)
*)
type operand =
  | Int64Const of int64
  | BoolConst of bool
  | FloatSymbol of float
  | StringSymbol of string
  | Register of vReg
  | FuncAddr of AST.functionId

(*
   Binary operations
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
   Unary operations
   Bitwise NOT: ~~~expr
*)
type unaryOp = Neg | Not | BitNot

(*
   Reference-count operation kind
*)
type rcKind = GenericHeap | StreamHeap | TaggedList | DictHeap | ClosureHeap

type cliOperation =
  | Execute
  | RunProcess
  | HostOS
  | HostArchitecture
  | Hostname
  | GetEnv
  | GetEnvironmentPacked
  | StdinState
  | SetEnv
  | UnsetEnv
  | DirectoryCurrent
  | DirectoryListPacked
  | FileIsDirectory
  | FileCreateExclusive
  | GetArgv
  | Kill
  | GetPid
  | StartupStack
  | ExecutableState
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
  | PosixProcInfo

(*
   Basic block label (defined early for use in Phi nodes)
*)
type label = Label of string

module LabelMap = Map.Make (struct
  type t = label

  let compare (Label left) (Label right) = StringOrder.compare left right
end)

module LabelSet = Set.Make (struct
  type t = label

  let compare (Label left) (Label right) = StringOrder.compare left right
end)

(*
   Instructions (non-control-flow)
   valueType for float/int distinction
   Direct function call (BL instruction)
   Tail call (B instruction, no return)
   Call through function pointer (BLR instruction)
   Indirect tail call (BR instruction)
   Allocate closure: (func_addr, caps...)
   Call through closure with hidden first arg
   Tail call through closure (BR instruction)
   Heap operations for tuples and other compound types
   Allocate heap memory
   Store at heap[addr+offset], valueType for float/int
   Load from heap[addr+offset]
   String operations
   Concatenate at least two strings with one allocation and ordered copies.
   Reference counting operations
   Increment ref count at [addr + payloadSize]
   Decrement ref count, free if zero
   Output operations (for main expression result printing)
   Print value with type-appropriate formatting
   Explicit stdout effect
   Read one line from stdin
   Print runtime error to stderr and exit with code 1
   Print a heap String error to stderr and exit with code 1
   File I/O intrinsics (generate syscalls)
   Read file, returns Result<Blob, String>
   Check if file exists, returns Bool
   Write Blob, returns Result<Unit, String>
   Append to file, returns Result<Unit, String>
   Delete file, returns Result<Unit, String>
   Create directory, returns Result<Unit, String>
   Set executable bit, returns Result<Unit, String>
   Write raw bytes to file
   Float intrinsics
   Square root: sqrt(x)
   Absolute value: |x|
   Negate: -x
   Convert Int64 to Float64
   Convert Float64 to Int64 (truncate)
   Copy Float64 bits to UInt64
   Raw memory intrinsics (internal, for HAMT implementation)
   Allocate raw bytes (no header), returns RawPtr
   Checked independent mapping with a private size prefix
   Manually free raw memory
   Release an independent mapping, not a heap block
   Read 8 bytes at offset, valueType for float
   Read 1 byte at offset (zero-extended)
   Write 8 unmanaged bytes at offset
   Write 1 unmanaged byte at offset
   Initialize typed 8-byte slot edge at offset
   Borrow raw backing pointer from String
   Reinterpret raw allocation as owned String
   Borrow raw backing pointer from Blob
   Reinterpret raw allocation as owned Blob
   Strip Dict tag bits, returning RawPtr
   Re-tag RawPtr as Dict
   Strip List tag bits, returning RawPtr
   Re-tag RawPtr as List
   Dynamic buffer reference counting at the value pointer
   Increment string ref count at [str]
   Decrement string ref count, free if zero
   Increment bytes ref count at [bytes]
   Decrement bytes ref count, free if zero
   Increment heap-backed Int ref count
   Decrement heap-backed Int ref count
   Random intrinsics
   Get 8 random bytes as Int64
   DateTime intrinsics
   Get the current UTC instant as 100ns Unix ticks
   Blocking typed native delay
   Float to String conversion
   Convert Float to heap String
   SSA phi node - merges values from different predecessor blocks
   Phi nodes must appear at the beginning of a basic block, before other instructions
   valueType distinguishes between integer (X registers) and float (D registers) phi nodes
   Coverage instrumentation - records that expression was executed
*)
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

(*
   Terminator instructions (control flow)
   Return from function
   Conditional branch
   Unconditional jump
*)
type terminator =
  | Ret of operand
  | Branch of operand * label * label
  | Jump of label

(*
   Basic block with label, instructions, and terminator
*)
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

(*
   Info about a single variant in a sum type (makes structure explicit)
*)
type variantInfo = {
  name : string;
  tag : int;
  payload : AST.semanticType option;
  fieldCount : int;
}

(*
   All variants for a sum type, with type parameters
*)
type typeVariants = { typeParams : string list; variants : variantInfo list }

(*
   Maps type name -> variant information
*)
type variantRegistry = typeVariants StringOrder.Map.t

(*
   Info about a single record field (makes structure explicit)
*)
type recordField = { name : string; typ : AST.semanticType }

(*
   Maps type name -> list of fields
*)
type recordRegistry = recordField list StringOrder.Map.t
type program = Program of functionDef list * variantRegistry * recordRegistry

(*
   Fresh register generator
*)
type regGen = RegGen of int

(*
   Fresh label generator
*)
type labelGen = LabelGen of int

let increment value = Int32.to_int (Int32.add (Int32.of_int value) 1l)

(*
   Generate a fresh virtual register
*)
let freshReg (RegGen value) = (VReg value, RegGen (increment value))

(*
   Generate a fresh label with optional function prefix (for uniqueness across functions)
*)
let freshLabelWithPrefix prefix (LabelGen value) =
  (Label (prefix ^ "_L" ^ string_of_int value), LabelGen (increment value))

(*
   Initial label generator
*)
let initialLabelGen = LabelGen 0
