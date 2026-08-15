// ANF.fs - A-Normal Form Intermediate Representation
//
// Defines the ANF (A-Normal Form) data structures.
//
// ANF is an intermediate representation where:
// - All intermediate computations are named with temporary variables
// - All operands to operations are simple (variables or literals, called "atoms")
// - Evaluation order is completely explicit through let-bindings
//
// This representation simplifies subsequent compiler passes by eliminating
// nested expressions.
//
// Example ANF for "2 + 3 * 4":
//   let tmp0 = 3
//   let tmp1 = 4
//   let tmp2 = tmp0 * tmp1
//   let tmp3 = 2
//   let tmp4 = tmp3 + tmp2
//   return tmp4

module ANF

/// Unique identifier for temporary variables
type TempId = TempId of int

/// Parameter with type information bundled together (makes invalid states unrepresentable)
type TypedParam = { Id: TempId; Type: AST.Type }

/// Integer value with explicit size - invalid states unrepresentable
/// Following "make invalid states unrepresentable" principle
type SizedInt =
    | Int8 of sbyte
    | Int16 of int16
    | Int32 of int32
    | Int64 of int64
    | UInt8 of byte
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64

/// Convert a SizedInt to the signed 64-bit payload used by MIR integer constants.
/// UInt64 values above Int64.MaxValue intentionally wrap to the same 64 bits.
let sizedIntToInt64 (si: SizedInt) : int64 =
    match si with
    | Int8 n -> int64 n
    | Int16 n -> int64 n
    | Int32 n -> int64 n
    | Int64 n -> n
    | UInt8 n -> int64 n
    | UInt16 n -> int64 n
    | UInt32 n -> int64 n
    | UInt64 n -> int64 n

/// Format a SizedInt as the corresponding source-level integer value.
let sizedIntToString (si: SizedInt) : string =
    match si with
    | Int8 n -> string n
    | Int16 n -> string n
    | Int32 n -> string n
    | Int64 n -> string n
    | UInt8 n -> string n
    | UInt16 n -> string n
    | UInt32 n -> string n
    | UInt64 n -> string n

/// Get the AST.Type corresponding to a SizedInt
let sizedIntToType (si: SizedInt) : AST.Type =
    match si with
    | Int8 _ -> AST.TInt8
    | Int16 _ -> AST.TInt16
    | Int32 _ -> AST.TInt32
    | Int64 _ -> AST.TInt64
    | UInt8 _ -> AST.TUInt8
    | UInt16 _ -> AST.TUInt16
    | UInt32 _ -> AST.TUInt32
    | UInt64 _ -> AST.TUInt64

/// Atomic expressions (cannot be decomposed further)
type Atom =
    | UnitLiteral            // Unit value: ()
    | IntLiteral of SizedInt // Integer with explicit size
    | BoolLiteral of bool
    | StringLiteral of string
    | FloatLiteral of float
    | Var of TempId
    | FuncRef of string  // Reference to a function by name (for higher-order functions)

/// Binary operations on atoms
type BinOp =
    // Arithmetic
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    // Bitwise
    | Shl     // << (left shift)
    | Shr     // >> (right shift)
    | BitAnd  // & (bitwise and)
    | BitOr   // ||| (bitwise or)
    | BitXor  // ^ (bitwise xor)
    // Comparisons
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte
    // Boolean
    | And
    | Or

/// Unary operations on atoms
type UnaryOp =
    | Neg
    | Not
    | BitNot  // Bitwise NOT: ~~~expr

/// Reference-count operation kind
type RcKind =
    | GenericHeap
    | StreamHeap
    | TaggedList
    | DictHeap
    | ClosureHeap

/// Runtime representation shape used to decide ownership behavior.
///
/// This is deliberately more specific than source-level heap-ness: values with
/// the same source type category can have different runtime ownership rules
/// depending on whether they are immediate, static, fixed-size, dynamically
/// sized, tagged, or unmanaged.
type RcShape =
    | Immediate
    | FixedBlock of payloadSize:int * fieldShapes:RcShape list
    | StreamRoot
    | BoxedSum of payloadSize:int * fieldShapes:(int * RcShape) list * variants:RcBoxedSumVariantShape list
    | RecursiveSumRef of sourceType:AST.Type
    | TaggedListShape of elementShape:RcShape
    | DictRoot of keyShape:RcShape * valueShape:RcShape
    | DynamicString
    | DynamicBlob
    | ClosureShape of captureShapes:RcShape list
    | StaticString
    | RawUnmanaged
and RcBoxedSumVariantShape = {
    Tag: int
    FieldShapes: (int * RcShape) list
}

/// Minimal sum metadata needed by RcShape without depending on later IR modules.
type RcSumShapeInfo = {
    TypeParams: string list
    Payloads: (int * AST.Type option) list
}

type RcSumShapeRegistry = Map<string, RcSumShapeInfo>

/// Root-level retain/release operation selected from a runtime shape.
type RcOperation =
    | FixedSizeRoot of payloadSize:int * kind:RcKind
    | DynamicStringBuffer
    | DynamicBlobBuffer

/// High-level storage management class selected from a runtime shape.
type RcStorageClass =
    | UnmanagedStorage
    | ManagedDynamicBuffer of operation:RcOperation
    | ManagedRcRoot of payloadSize:int * kind:RcKind

/// Structured release plan selected from a runtime shape.
///
/// Backends can consume this instead of rediscovering nested ownership by
/// pattern matching on source types. RootRelease describes the refcounted root;
/// the nested payload plan describes extra work that must happen only when the
/// root refcount reaches zero.
type RcReleasePlan =
    | NoReleasePlan
    | DynamicBufferRelease of operation:RcOperation
    | RecursiveRelease of sourceType:AST.Type
    | RootRelease of payloadSize:int * kind:RcKind * payload:RcPayloadReleasePlan
and RcPayloadReleasePlan =
    | NoPayloadRelease
    | FixedBlockPayloadRelease of payloadSize:int * fieldReleases:RcFieldRelease list
    | BoxedSumPayloadRelease of payloadSize:int * fieldReleases:RcFieldRelease list * variants:RcBoxedSumVariantRelease list
    | TaggedListPayloadRelease of elementRelease:RcReleasePlan
    | DictPayloadRelease of keyRelease:RcReleasePlan * valueRelease:RcReleasePlan
    | ClosurePayloadRelease of captureReleases:RcFieldRelease list
and RcFieldRelease =
    | FieldRelease of offset:int * release:RcReleasePlan
and RcBoxedSumVariantRelease = {
    Tag: int
    FieldReleases: RcFieldRelease list
}

/// Metadata carried by refcount operations after ownership insertion.
///
/// ReleasePlan is the backend-facing source of truth for retain/release helper
/// selection. SourceType is retained as contextual metadata for diagnostics and
/// focused compiler-pass tests; backend cleanup must not reconstruct release
/// behavior from it.
type RcMetadata = {
    ReleasePlan: RcReleasePlan option
    SourceType: AST.Type option
}

/// Function return ownership convention
type ReturnOwnership =
    | OwnedReturn
    | BorrowedReturn

/// Typed native effects retained after the portable Stdlib.Cli wrappers lower.
type CliOperation =
    | Execute
    | HostOS
    | GetEnv
    | Kill
    | GetPid
    | GetUid
    | CpuCount
    | CurrentUser
    | SpawnProcess
    | ProcessIO
    | TerminateProcess

/// Complex expressions (produce values)
type CExpr =
    | Atom of Atom
    | TypedAtom of Atom * AST.Type  // Atom with explicit type (for pattern matching where inferred types would be wrong)
    | Prim of BinOp * Atom * Atom
    | UnaryPrim of UnaryOp * Atom
    | IfValue of cond:Atom * thenValue:Atom * elseValue:Atom  // If-expression that produces a value
    | Call of funcName:string * args:Atom list  // Function call (direct: BL instruction)
    | BorrowedCall of funcName:string * args:Atom list  // Function call that returns a borrowed/aliased value
    | TailCall of funcName:string * args:Atom list  // Tail call (direct: B instruction, no return)
    | IndirectCall of func:Atom * args:Atom list  // Call through function variable (BLR instruction)
    | IndirectTailCall of func:Atom * args:Atom list  // Tail call through function variable (BR instruction)
    | ClosureAlloc of funcName:string * captures:Atom list  // Allocate closure: (func_addr, cap1, cap2, ...)
    | ClosureCall of closure:Atom * args:Atom list  // Call through closure, passing closure as hidden first arg
    | ClosureTailCall of closure:Atom * args:Atom list  // Tail call through closure (BR instruction)
    | TupleAlloc of Atom list                   // Create tuple: (a, b, c)
    | TupleGet of tuple:Atom * index:int        // Get tuple element: t.0
    // String operations (heap-allocating)
    | StringConcat of left:Atom * right:Atom    // Concatenate strings: s1 ++ s2
    // Reference counting operations
    | RefCountInc of Atom * payloadSize:int * kind:RcKind * metadata:RcMetadata option    // Increment ref count of heap value
    | RefCountDec of Atom * payloadSize:int * kind:RcKind * metadata:RcMetadata option    // Decrement ref count, free if zero
    // Output operations (for main expression result)
    | Print of Atom * AST.Type                 // Print value with type-appropriate formatting
    | StdoutWrite of value:Atom * appendNewline:bool // Explicit stdout effect; returns Unit
    | StdinReadLine                            // Read one UTF-8 line from stdin; returns String
    | RuntimeError of message:string           // Print runtime error to stderr and exit with code 1
    | RuntimeErrorString of message:Atom       // Print a language String error to stderr and exit with code 1
    // File I/O intrinsics (generate syscalls)
    | FileReadText of path:Atom               // Read file, returns Result<String, String>
    | FileExists of path:Atom                 // Check if file exists, returns Bool
    | FileWriteText of path:Atom * content:Atom  // Write file, returns Result<Unit, String>
    | FileAppendText of path:Atom * content:Atom // Append to file, returns Result<Unit, String>
    | FileDelete of path:Atom                     // Delete file, returns Result<Unit, String>
    | FileSetExecutable of path:Atom             // Set executable bit, returns Result<Unit, String>
    | FileWriteFromPtr of path:Atom * ptr:Atom * length:Atom  // Write raw bytes from pointer to file
    // Float intrinsics
    | FloatSqrt of Atom                       // Square root: sqrt(x)
    | FloatAbs of Atom                        // Absolute value: |x|
    | FloatNeg of Atom                        // Negate: -x
    | Int64ToFloat of Atom                    // Convert Int64 to Float64
    | FloatToInt64 of Atom                    // Convert Float64 to Int64 (truncate)
    | FloatToBits of Atom                     // Copy Float64 bits to UInt64
    // Raw memory intrinsics (internal, for HAMT implementation)
    | RawAlloc of numBytes:Atom               // Allocate raw bytes (no header), returns RawPtr
    | RawFree of ptr:Atom                     // Manually free raw memory
    | RawGet of ptr:Atom * byteOffset:Atom * valueType:AST.Type option  // Read 8 bytes at offset, valueType for float
    | RawTake of ptr:Atom * byteOffset:Atom * valueType:AST.Type option // Transfer a typed slot edge to the result
    | RawGetByte of ptr:Atom * byteOffset:Atom  // Read 1 byte at offset, returns Int64 (zero-extended)
    | RawWriteWord of ptr:Atom * byteOffset:Atom * value:Atom  // Write 8 unmanaged bytes at offset
    | RawWriteByte of ptr:Atom * byteOffset:Atom * value:Atom  // Write 1 unmanaged byte at offset
    | RawSlotInit of ptr:Atom * byteOffset:Atom * value:Atom * valueType:AST.Type  // Initialize typed 8-byte slot edge at offset
    | StringToRawPtr of value:Atom              // Borrow raw backing pointer from String
    | RawPtrToString of ptr:Atom                // Reinterpret raw allocation as owned String
    | BlobToRawPtr of value:Atom               // Borrow raw backing pointer from Blob
    | RawPtrToBlob of ptr:Atom                 // Reinterpret raw allocation as owned Blob
    | DictToRawPtr of dict:Atom                 // Strip Dict tag bits, returning RawPtr
    | RawPtrToDict of ptr:Atom * tag:Atom * dictType:AST.Type  // Re-tag RawPtr as Dict
    | ListToRawPtr of list:Atom                 // Strip List tag bits, returning RawPtr
    | RawPtrToList of ptr:Atom * tag:Atom * listType:AST.Type  // Re-tag RawPtr as List
    // Dynamic buffer reference counting (at offset computed from length)
    | RefCountIncString of Atom               // Increment string ref count
    | RefCountDecString of Atom               // Decrement string ref count, free if zero
    | RefCountIncBlob of Atom                // Increment bytes ref count
    | RefCountDecBlob of Atom                // Decrement bytes ref count, free if zero
    // Random intrinsics
    | RandomInt64                             // Get 8 random bytes as Int64
    // DateTime intrinsics
    | DateTimeNow                             // Get the current UTC instant as 100ns Unix ticks
    | Sleep of delayMs:Atom                   // Blocking typed native delay in milliseconds
    | CliNative of operation:CliOperation * args:Atom list
    // Float to String conversion
    | FloatToString of Atom                   // Convert Float to heap String

/// ANF expressions with explicit sequencing
type AExpr =
    | Let of TempId * CExpr * AExpr
    | Return of Atom
    | If of cond:Atom * thenBranch:AExpr * elseBranch:AExpr

/// ANF function definition
type Function = {
    Name: string
    TypedParams: TypedParam list  // Parameter IDs with their types bundled
    ReturnType: AST.Type
    ReturnOwnership: ReturnOwnership
    Body: AExpr
}

/// ANF program (functions and main expression)
type Program = Program of functions:Function list * main:AExpr

/// Fresh variable generator (functional style)
type VarGen = VarGen of int

/// Generate a fresh temporary variable
let freshVar (VarGen n) : TempId * VarGen =
    (TempId n, VarGen (n + 1))

/// Initial variable generator
let initialVarGen = VarGen 0

/// Type map for tracking TempId -> Type mappings
/// Used by reference counting pass to determine which values are heap-allocated
type TypeMap = Map<TempId, AST.Type>

/// Program with type information for reference counting
type TypedProgram = {
    Program: Program
    TypeMap: TypeMap
}

/// Classify a source type into its current runtime RC representation shape.
///
/// The classifier is intentionally pure and side-effect free. Ownership
/// insertion and backend helper selection use this as the source of truth for
/// runtime retain/release shape decisions.
let rec rcShapeOfType (typeReg: Map<string, (string * AST.Type) list>) (t: AST.Type) : RcShape =
    match t with
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TInt128
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128
    | AST.TBool
    | AST.TFloat64
    | AST.TDateTime
    | AST.TUnit
    | AST.TRuntimeError
    | AST.TVar _ ->
        Immediate
    | AST.TTuple elemTypes ->
        let fieldShapes = elemTypes |> List.map (rcShapeOfType typeReg)
        FixedBlock (List.length elemTypes * 8, fieldShapes)
    | AST.TEnumFields fieldTypes ->
        let fieldShapes = fieldTypes |> List.map (rcShapeOfType typeReg)
        FixedBlock (List.length fieldTypes * 8, fieldShapes)
    | AST.TRecord (name, _) ->
        match Map.tryFind name typeReg with
        | Some fields ->
            let fieldShapes =
                fields
                |> List.map (fun (_, fieldType) -> rcShapeOfType typeReg fieldType)
            FixedBlock (List.length fields * 8, fieldShapes)
        | None ->
            Crash.crash $"rcShapeOfType: Record type '{name}' not found in typeReg"
    | AST.TSum (_, []) ->
        Immediate
    | AST.TSum (_, [payloadType]) ->
        BoxedSum (16, [(8, rcShapeOfType typeReg payloadType)], [])
    | AST.TSum _ ->
        BoxedSum (16, [], [])
    | AST.TList elemType ->
        TaggedListShape (rcShapeOfType typeReg elemType)
    | AST.TStream _ -> StreamRoot
    | AST.TDict (keyType, valueType) ->
        DictRoot (rcShapeOfType typeReg keyType, rcShapeOfType typeReg valueType)
    | AST.TString
    | AST.TChar
    | AST.TInt ->
        DynamicString
    | AST.TBlob ->
        DynamicBlob
    | AST.TFunction _ ->
        ClosureShape []
    | AST.TRawPtr ->
        RawUnmanaged

let private rcShapeTypeSubstitution (typeParams: string list) (typeArgs: AST.Type list) : Map<string, AST.Type> =
    if List.length typeParams = List.length typeArgs then
        List.zip typeParams typeArgs |> Map.ofList
    else
        Crash.crash $"rcShapeOfTypeWithSums: sum type argument mismatch: params={typeParams.Length}, args={typeArgs.Length}"

let private collectTypeVarsInOrder (typ: AST.Type) : string list =
    let rec collect t =
        match t with
        | AST.TVar name -> [name]
        | AST.TTuple elemTypes -> elemTypes |> List.collect collect
        | AST.TEnumFields fieldTypes -> fieldTypes |> List.collect collect
        | AST.TRecord (_, typeArgs) -> typeArgs |> List.collect collect
        | AST.TList elemType -> collect elemType
        | AST.TStream elemType -> collect elemType
        | AST.TDict (keyType, valueType) -> collect keyType @ collect valueType
        | AST.TSum (_, typeArgs) -> typeArgs |> List.collect collect
        | AST.TFunction (paramTypes, returnType) ->
            (paramTypes |> List.collect collect) @ collect returnType
        | AST.TInt8
        | AST.TInt16
        | AST.TInt32
        | AST.TInt64
        | AST.TInt128
        | AST.TInt
        | AST.TUInt8
        | AST.TUInt16
        | AST.TUInt32
        | AST.TUInt64
        | AST.TUInt128
        | AST.TBool
        | AST.TFloat64
        | AST.TString
        | AST.TBlob
        | AST.TChar
        | AST.TDateTime
        | AST.TUnit
        | AST.TRawPtr
        | AST.TRuntimeError ->
            []
    collect typ |> List.distinct

let rec private applyRcShapeTypeSubstitution (subst: Map<string, AST.Type>) (typ: AST.Type) : AST.Type =
    match typ with
    | AST.TVar name ->
        match Map.tryFind name subst with
        | Some concrete -> concrete
        | None -> typ
    | AST.TTuple elemTypes ->
        AST.TTuple (elemTypes |> List.map (applyRcShapeTypeSubstitution subst))
    | AST.TEnumFields fieldTypes ->
        AST.TEnumFields (fieldTypes |> List.map (applyRcShapeTypeSubstitution subst))
    | AST.TRecord (name, typeArgs) ->
        AST.TRecord (name, typeArgs |> List.map (applyRcShapeTypeSubstitution subst))
    | AST.TList elemType ->
        AST.TList (applyRcShapeTypeSubstitution subst elemType)
    | AST.TStream elemType ->
        AST.TStream (applyRcShapeTypeSubstitution subst elemType)
    | AST.TDict (keyType, valueType) ->
        AST.TDict (applyRcShapeTypeSubstitution subst keyType, applyRcShapeTypeSubstitution subst valueType)
    | AST.TSum (name, typeArgs) ->
        AST.TSum (name, typeArgs |> List.map (applyRcShapeTypeSubstitution subst))
    | AST.TFunction (paramTypes, returnType) ->
        AST.TFunction (
            paramTypes |> List.map (applyRcShapeTypeSubstitution subst),
            applyRcShapeTypeSubstitution subst returnType
        )
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt64
    | AST.TInt128
    | AST.TInt
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128
    | AST.TBool
    | AST.TFloat64
    | AST.TString
    | AST.TBlob
    | AST.TChar
    | AST.TDateTime
    | AST.TUnit
    | AST.TRawPtr
    | AST.TRuntimeError ->
        typ

/// Classify a source type using record metadata and optional named-sum metadata.
let rcShapeOfTypeWithSums
    (typeReg: Map<string, (string * AST.Type) list>)
    (sumReg: RcSumShapeRegistry)
    (t: AST.Type)
    : RcShape =
    let rec classify (expandingSums: Set<string>) (t: AST.Type) : RcShape =
        match t with
        | AST.TTuple elemTypes ->
            FixedBlock (List.length elemTypes * 8, elemTypes |> List.map (classify expandingSums))
        | AST.TEnumFields fieldTypes ->
            FixedBlock (List.length fieldTypes * 8, fieldTypes |> List.map (classify expandingSums))
        | AST.TRecord (name, typeArgs) ->
            match Map.tryFind name typeReg with
            | Some fields ->
                let typeParams =
                    fields
                    |> List.collect (fun (_, fieldType) -> collectTypeVarsInOrder fieldType)
                    |> List.distinct
                let subst = rcShapeTypeSubstitution typeParams typeArgs
                let fieldShapes =
                    fields
                    |> List.map (fun (_, fieldType) ->
                        fieldType |> applyRcShapeTypeSubstitution subst |> classify expandingSums)
                FixedBlock (List.length fields * 8, fieldShapes)
            | None ->
                Crash.crash $"rcShapeOfTypeWithSums: Record type '{name}' not found in typeReg"
        | AST.TSum (name, typeArgs) ->
            if Set.contains name expandingSums then
                RecursiveSumRef (AST.TSum (name, typeArgs))
            else
                match Map.tryFind name sumReg with
                | Some sumInfo ->
                    let subst = rcShapeTypeSubstitution sumInfo.TypeParams typeArgs
                    let expandingSums = Set.add name expandingSums

                    let variantShapes =
                        sumInfo.Payloads
                        |> List.map (fun maybePayload ->
                            match maybePayload with
                            | tag, Some payload ->
                                let payloadShape = payload |> applyRcShapeTypeSubstitution subst |> classify expandingSums
                                { Tag = tag; FieldShapes = [(8, payloadShape)] }
                            | tag, None ->
                                { Tag = tag; FieldShapes = [] })

                    let hasPayloadVariant =
                        sumInfo.Payloads
                        |> List.exists (fun (_, payload) -> Option.isSome payload)

                    if hasPayloadVariant then
                        let fieldShapes =
                            variantShapes
                            |> List.collect (fun variant -> variant.FieldShapes)

                        BoxedSum (16, fieldShapes, variantShapes)
                    else
                        Immediate
                | None ->
                    Crash.crash $"rcShapeOfTypeWithSums: Sum type '{name}' not found in sumReg"
        | AST.TList elemType ->
            TaggedListShape (classify expandingSums elemType)
        | AST.TStream _ -> StreamRoot
        | AST.TDict (keyType, valueType) ->
            DictRoot (classify expandingSums keyType, classify expandingSums valueType)
        | AST.TFunction _ ->
            ClosureShape []
        | AST.TString
        | AST.TChar
        | AST.TInt ->
            DynamicString
        | AST.TBlob ->
            DynamicBlob
        | AST.TRawPtr ->
            RawUnmanaged
        | AST.TInt8
        | AST.TInt16
        | AST.TInt32
        | AST.TInt64
        | AST.TInt128
        | AST.TUInt8
        | AST.TUInt16
        | AST.TUInt32
        | AST.TUInt64
        | AST.TUInt128
        | AST.TBool
        | AST.TFloat64
        | AST.TDateTime
        | AST.TUnit
        | AST.TRuntimeError
        | AST.TVar _ ->
            Immediate

    classify Set.empty t

/// True when a runtime shape can own managed memory that must be released when
/// an owning binding leaves scope.
let rcShapeNeedsOwnedScopeRelease (shape: RcShape) : bool =
    match shape with
    | Immediate
    | StaticString
    | RawUnmanaged ->
        false
    | DynamicString
    | DynamicBlob
    | FixedBlock _
    | StreamRoot
    | BoxedSum _
    | RecursiveSumRef _
    | TaggedListShape _
    | DictRoot _
    | ClosureShape _ ->
        true

/// True when a shape is managed through a fixed-size or tagged RC root rather
/// than a dynamic-buffer helper or an unmanaged representation.
let rcShapeIsRootManaged (shape: RcShape) : bool =
    match shape with
    | FixedBlock _
    | StreamRoot
    | BoxedSum _
    | RecursiveSumRef _
    | TaggedListShape _
    | DictRoot _
    | ClosureShape _ ->
        true
    | Immediate
    | DynamicString
    | DynamicBlob
    | StaticString
    | RawUnmanaged ->
        false

/// True when releasing a value of this shape can require walking owned payload
/// fields, captures, list leaves, or dict leaf entries in addition to releasing
/// the root allocation itself.
let rec rcShapeNeedsRecursiveRelease (shape: RcShape) : bool =
    match shape with
    | FixedBlock (_, fieldShapes) ->
        fieldShapes |> List.exists rcShapeNeedsOwnedScopeRelease
    | StreamRoot -> true
    | BoxedSum (_, fieldShapes, _) ->
        fieldShapes
        |> List.exists (fun (_, fieldShape) -> rcShapeNeedsOwnedScopeRelease fieldShape)
    | RecursiveSumRef _ ->
        true
    | TaggedListShape elementShape ->
        rcShapeNeedsOwnedScopeRelease elementShape
    | DictRoot (keyShape, valueShape) ->
        rcShapeNeedsOwnedScopeRelease keyShape
        || rcShapeNeedsOwnedScopeRelease valueShape
    | ClosureShape captureShapes ->
        captureShapes |> List.exists rcShapeNeedsOwnedScopeRelease
    | Immediate
    | DynamicString
    | DynamicBlob
    | StaticString
    | RawUnmanaged ->
        false

/// Dispatch kind for fixed-size/tagged RC roots. Dynamic buffers use their own
/// string/bytes operations, so they intentionally do not have a root kind here.
let rcShapeRootKind (shape: RcShape) : RcKind option =
    match shape with
    | FixedBlock _
    | BoxedSum _
    | RecursiveSumRef _ ->
        Some GenericHeap
    | StreamRoot -> Some StreamHeap
    | TaggedListShape _ ->
        Some TaggedList
    | DictRoot _ ->
        Some DictHeap
    | ClosureShape _ ->
        Some ClosureHeap
    | Immediate
    | DynamicString
    | DynamicBlob
    | StaticString
    | RawUnmanaged ->
        None

/// Payload size for fixed-size/tagged RC roots.
let rcShapePayloadSize (shape: RcShape) : int option =
    match shape with
    | FixedBlock (payloadSize, _)
    | BoxedSum (payloadSize, _, _) ->
        Some payloadSize
    | StreamRoot -> Some 24
    | RecursiveSumRef _ ->
        Some 16
    | TaggedListShape _ ->
        Some 24
    | DictRoot _ ->
        Some 8
    | ClosureShape _ ->
        Some 0
    | Immediate
    | DynamicString
    | DynamicBlob
    | StaticString
    | RawUnmanaged ->
        None

/// Storage class for deciding whether a value is unmanaged, managed by a
/// dynamic-buffer helper, or managed by a fixed/tagged RC root helper.
let rcShapeStorageClass (shape: RcShape) : RcStorageClass =
    match shape with
    | DynamicString ->
        ManagedDynamicBuffer DynamicStringBuffer
    | DynamicBlob ->
        ManagedDynamicBuffer DynamicBlobBuffer
    | _ ->
        match rcShapePayloadSize shape, rcShapeRootKind shape with
        | Some payloadSize, Some kind ->
            ManagedRcRoot (payloadSize, kind)
        | _ ->
            UnmanagedStorage

/// True when a value is represented by an RC root whose ownership can be
/// transferred to another aggregate or helper call.
let rcShapeIsOwnershipTransferRoot (shape: RcShape) : bool =
    match rcShapeStorageClass shape with
    | ManagedRcRoot _ ->
        true
    | ManagedDynamicBuffer _
    | UnmanagedStorage ->
        false

/// Retain operation for an owned or borrowed value of the given shape.
let rcShapeRetainOperation (shape: RcShape) : RcOperation option =
    match rcShapeStorageClass shape with
    | ManagedDynamicBuffer operation ->
        Some operation
    | ManagedRcRoot (payloadSize, kind) ->
        Some (FixedSizeRoot (payloadSize, kind))
    | UnmanagedStorage ->
        None

/// Release operation for an owned value of the given shape.
let rcShapeReleaseOperation (shape: RcShape) : RcOperation option =
    if rcShapeNeedsOwnedScopeRelease shape then
        rcShapeRetainOperation shape
    else
        None

/// True when a borrowed value of this shape needs a retain before it can be
/// returned or otherwise materialized as a new owned value.
let rcShapeNeedsBorrowedRetain (shape: RcShape) : bool =
    rcShapeRetainOperation shape |> Option.isSome

/// True when a normal owning binding of this shape should receive an automatic
/// decrement from RC insertion. Closure roots are handled by closure-producing
/// expressions so aliases of function-typed values do not double-release.
let rcShapeNeedsAutomaticBindingDec (shape: RcShape) : bool =
    match shape with
    | ClosureShape _ ->
        false
    | _ ->
        rcShapeNeedsOwnedScopeRelease shape

/// True when a borrowed alias of this shape carries a managed root identity
/// that should be preserved by type inference. Closure aliases are excluded
/// because closure-producing expressions own their lifetime separately.
let rcShapeNeedsManagedAliasRootPreservation (shape: RcShape) : bool =
    match rcShapeStorageClass shape with
    | ManagedRcRoot (_, ClosureHeap) ->
        false
    | ManagedRcRoot _ ->
        true
    | ManagedDynamicBuffer _
    | UnmanagedStorage ->
        false

/// Release plan for a value with the given runtime shape.
let rec rcShapeReleasePlan (shape: RcShape) : RcReleasePlan =
    let releasePlansAtOffsets (fields: (int * RcShape) list) : RcFieldRelease list =
        fields
        |> List.choose (fun (offset, fieldShape) ->
            match rcShapeReleasePlan fieldShape with
            | NoReleasePlan ->
                None
            | plan ->
                Some (FieldRelease (offset, plan)))

    let fieldReleasePlans (fieldShapes: RcShape list) : RcFieldRelease list =
        fieldShapes
        |> List.mapi (fun index fieldShape -> (index * 8, fieldShape))
        |> releasePlansAtOffsets

    let rootPayloadPlan (rootShape: RcShape) : RcPayloadReleasePlan =
        match rootShape with
        | FixedBlock (payloadSize, fieldShapes) ->
            FixedBlockPayloadRelease (payloadSize, fieldReleasePlans fieldShapes)
        | StreamRoot ->
            FixedBlockPayloadRelease (24, fieldReleasePlans [Immediate; ClosureShape []; ClosureShape []])
        | BoxedSum (payloadSize, fieldShapes, variants) ->
            let variantReleases =
                variants
                |> List.map (fun variant ->
                    let releases =
                        variant.FieldShapes
                        |> releasePlansAtOffsets

                    { Tag = variant.Tag; FieldReleases = releases })

            BoxedSumPayloadRelease (payloadSize, releasePlansAtOffsets fieldShapes, variantReleases)
        | TaggedListShape elementShape ->
            TaggedListPayloadRelease (rcShapeReleasePlan elementShape)
        | DictRoot (keyShape, valueShape) ->
            DictPayloadRelease (rcShapeReleasePlan keyShape, rcShapeReleasePlan valueShape)
        | ClosureShape captureShapes ->
            ClosurePayloadRelease (fieldReleasePlans captureShapes)
        | RecursiveSumRef _ ->
            NoPayloadRelease
        | Immediate
        | DynamicString
        | DynamicBlob
        | StaticString
        | RawUnmanaged ->
            NoPayloadRelease

    match rcShapeStorageClass shape with
    | ManagedRcRoot (payloadSize, kind) ->
        match shape with
        | RecursiveSumRef sourceType -> RecursiveRelease sourceType
        | _ -> RootRelease (payloadSize, kind, rootPayloadPlan shape)
    | UnmanagedStorage ->
        NoReleasePlan
    | ManagedDynamicBuffer operation ->
        DynamicBufferRelease operation

/// Release plan for a source type using the current representation registry.
let rec rcReleasePlanOfType (typeReg: Map<string, (string * AST.Type) list>) (t: AST.Type) : RcReleasePlan =
    t |> rcShapeOfType typeReg |> rcShapeReleasePlan

/// Release plan for a source type using record and named-sum metadata.
let rec rcReleasePlanOfTypeWithSums
    (typeReg: Map<string, (string * AST.Type) list>)
    (sumReg: RcSumShapeRegistry)
    (t: AST.Type)
    : RcReleasePlan =
    t |> rcShapeOfTypeWithSums typeReg sumReg |> rcShapeReleasePlan

/// Collect the concrete recursive sum roots referenced by a finite release plan.
let rec recursiveReleaseTypes (releasePlan: RcReleasePlan) : Set<AST.Type> =
    let fromFields fieldReleases =
        fieldReleases
        |> List.map (fun (FieldRelease (_, fieldPlan)) -> recursiveReleaseTypes fieldPlan)
        |> List.fold Set.union Set.empty

    match releasePlan with
    | RecursiveRelease sourceType ->
        Set.singleton sourceType
    | RootRelease (_, _, FixedBlockPayloadRelease (_, fieldReleases))
    | RootRelease (_, _, BoxedSumPayloadRelease (_, fieldReleases, _))
    | RootRelease (_, _, ClosurePayloadRelease fieldReleases) ->
        fromFields fieldReleases
    | RootRelease (_, _, TaggedListPayloadRelease elementRelease) ->
        recursiveReleaseTypes elementRelease
    | RootRelease (_, _, DictPayloadRelease (keyRelease, valueRelease)) ->
        Set.union (recursiveReleaseTypes keyRelease) (recursiveReleaseTypes valueRelease)
    | RootRelease (_, _, NoPayloadRelease)
    | DynamicBufferRelease _
    | NoReleasePlan ->
        Set.empty

// ============================================================================
// Coverage Types
// ============================================================================

/// Unique expression ID for coverage tracking
type ExprId = int

/// Expression ID generator (functional style, like VarGen)
type ExprIdGen = ExprIdGen of int

/// Generate a fresh expression ID
let freshExprId (ExprIdGen n) : ExprId * ExprIdGen =
    (n, ExprIdGen (n + 1))

/// Initial expression ID generator
let initialExprIdGen = ExprIdGen 0

/// Coverage mapping: tracks expression descriptions for reporting
type CoverageMapping = {
    /// ExprId -> description string (e.g., "List.map: Call filter")
    Descriptions: Map<int, string>
    /// Total number of expressions tracked
    TotalExpressions: int
}

/// Empty coverage mapping
let emptyCoverageMapping : CoverageMapping = {
    Descriptions = Map.empty
    TotalExpressions = 0
}

/// Add an expression to the coverage mapping
let addCoverageEntry (exprId: ExprId) (description: string) (mapping: CoverageMapping) : CoverageMapping =
    { mapping with
        Descriptions = Map.add exprId description mapping.Descriptions
        TotalExpressions = max mapping.TotalExpressions (exprId + 1) }
