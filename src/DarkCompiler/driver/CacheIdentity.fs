// CacheIdentity.fs - Define stable dependency and native-code cache identities.

module CompilationCacheIdentity

open ARM64CodeGenTypes
open CodeGen
open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Collections.Generic
open CompilerOptions

/// MIR virtual registers are local to a function, but ANF assigns their source
/// TempIds from a program-wide generator. Normalize that irrelevant offset for
/// the per-function SSA cache so the same dependency remains reusable when an
/// unrelated function precedes it in a different compilation unit.
let internal normalizeMirFunctionRegisterOffset
    (func: MIR.Function)
    : MIR.Function =
    let minimumRegisterId =
        let includeReg minimum (MIR.VReg id) = min minimum id
        let includeInstr minimum instr =
            let minimum =
                match MIROptimizationFacts.getInstrDest instr with
                | Some dest -> includeReg minimum dest
                | None -> minimum
            let minimum =
                MIROptimizationFacts.foldInstrUses includeReg minimum instr
            match instr with
            | MIR.StdoutWrite (effectId, _, _)
            | MIR.Sleep (effectId, _, _) -> min minimum effectId
            | _ -> minimum
        let minimum =
            func.TypedParams
            |> List.fold (fun acc param -> includeReg acc param.Reg) System.Int32.MaxValue
        let minimum = func.FloatRegs |> Set.fold min minimum
        func.CFG.Blocks
        |> Map.fold (fun minimum _ block ->
            let minimum = block.Instrs |> List.fold includeInstr minimum
            MIROptimizationFacts.foldTerminatorUses includeReg minimum block.Terminator) minimum

    match minimumRegisterId with
    | minimum when minimum = System.Int32.MaxValue || minimum = 0 -> func
    | offset ->
        let shiftReg (MIR.VReg id) = MIR.VReg (id - offset)
        let shiftEffectId id =
            let (MIR.VReg shifted) = shiftReg (MIR.VReg id)
            shifted
        let shiftOperand = function
            | MIR.Register reg -> MIR.Register (shiftReg reg)
            | operand -> operand
        let shiftOperands = List.map shiftOperand
        let shiftInstr = function
            | MIR.Mov (dest, src, valueType) ->
                MIR.Mov (shiftReg dest, shiftOperand src, valueType)
            | MIR.BinOp (dest, op, left, right, operandType) ->
                MIR.BinOp (
                    shiftReg dest,
                    op,
                    shiftOperand left,
                    shiftOperand right,
                    operandType)
            | MIR.UnaryOp (dest, op, src) ->
                MIR.UnaryOp (shiftReg dest, op, shiftOperand src)
            | MIR.Call (dest, name, args, argTypes, returnType) ->
                MIR.Call (shiftReg dest, name, shiftOperands args, argTypes, returnType)
            | MIR.TailCall (name, args, argTypes, returnType) ->
                MIR.TailCall (name, shiftOperands args, argTypes, returnType)
            | MIR.IndirectCall (dest, callee, args, argTypes, returnType) ->
                MIR.IndirectCall (
                    shiftReg dest,
                    shiftOperand callee,
                    shiftOperands args,
                    argTypes,
                    returnType)
            | MIR.IndirectTailCall (callee, args, argTypes, returnType) ->
                MIR.IndirectTailCall (
                    shiftOperand callee,
                    shiftOperands args,
                    argTypes,
                    returnType)
            | MIR.ClosureAlloc (dest, name, captures) ->
                MIR.ClosureAlloc (shiftReg dest, name, shiftOperands captures)
            | MIR.ClosureCall (dest, closure, args, argTypes, returnType) ->
                MIR.ClosureCall (
                    shiftReg dest,
                    shiftOperand closure,
                    shiftOperands args,
                    argTypes,
                    returnType)
            | MIR.ClosureTailCall (closure, args, argTypes) ->
                MIR.ClosureTailCall (shiftOperand closure, shiftOperands args, argTypes)
            | MIR.HeapAlloc (dest, sizeBytes) ->
                MIR.HeapAlloc (shiftReg dest, sizeBytes)
            | MIR.HeapStore (addr, byteOffset, src, valueType) ->
                MIR.HeapStore (shiftReg addr, byteOffset, shiftOperand src, valueType)
            | MIR.HeapLoad (dest, addr, byteOffset, valueType) ->
                MIR.HeapLoad (shiftReg dest, shiftReg addr, byteOffset, valueType)
            | MIR.StringConcat (dest, first, second, remaining) ->
                MIR.StringConcat (
                    shiftReg dest,
                    shiftOperand first,
                    shiftOperand second,
                    shiftOperands remaining)
            | MIR.CanonicalBufferEq (dest, kind, left, right) ->
                MIR.CanonicalBufferEq (
                    shiftReg dest,
                    kind,
                    shiftOperand left,
                    shiftOperand right)
            | MIR.RefCountInc (addr, payloadSize, kind, metadata) ->
                MIR.RefCountInc (shiftReg addr, payloadSize, kind, metadata)
            | MIR.RefCountDec (addr, payloadSize, kind, metadata) ->
                MIR.RefCountDec (shiftReg addr, payloadSize, kind, metadata)
            | MIR.Print (src, valueType) ->
                MIR.Print (shiftOperand src, valueType)
            | MIR.StdoutWrite (effectId, src, appendNewline) ->
                MIR.StdoutWrite (
                    shiftEffectId effectId,
                    shiftOperand src,
                    appendNewline)
            | MIR.StdinReadLine dest -> MIR.StdinReadLine (shiftReg dest)
            | MIR.RuntimeError message -> MIR.RuntimeError message
            | MIR.RuntimeErrorString message ->
                MIR.RuntimeErrorString (shiftOperand message)
            | MIR.FileReadBlob (dest, path) ->
                MIR.FileReadBlob (shiftReg dest, shiftOperand path)
            | MIR.FileExists (dest, path) ->
                MIR.FileExists (shiftReg dest, shiftOperand path)
            | MIR.FileWriteBlob (dest, path, content) ->
                MIR.FileWriteBlob (
                    shiftReg dest,
                    shiftOperand path,
                    shiftOperand content)
            | MIR.FileAppendText (dest, path, content) ->
                MIR.FileAppendText (
                    shiftReg dest,
                    shiftOperand path,
                    shiftOperand content)
            | MIR.FileDelete (dest, path) ->
                MIR.FileDelete (shiftReg dest, shiftOperand path)
            | MIR.FileCreateDirectory (dest, path) ->
                MIR.FileCreateDirectory (shiftReg dest, shiftOperand path)
            | MIR.FileSetExecutable (dest, path) ->
                MIR.FileSetExecutable (shiftReg dest, shiftOperand path)
            | MIR.FileWriteFromPtr (dest, path, ptr, length) ->
                MIR.FileWriteFromPtr (
                    shiftReg dest,
                    shiftOperand path,
                    shiftOperand ptr,
                    shiftOperand length)
            | MIR.FloatSqrt (dest, src) ->
                MIR.FloatSqrt (shiftReg dest, shiftOperand src)
            | MIR.FloatAbs (dest, src) ->
                MIR.FloatAbs (shiftReg dest, shiftOperand src)
            | MIR.FloatNeg (dest, src) ->
                MIR.FloatNeg (shiftReg dest, shiftOperand src)
            | MIR.Int64ToFloat (dest, src) ->
                MIR.Int64ToFloat (shiftReg dest, shiftOperand src)
            | MIR.FloatToInt64 (dest, src) ->
                MIR.FloatToInt64 (shiftReg dest, shiftOperand src)
            | MIR.FloatToBits (dest, src) ->
                MIR.FloatToBits (shiftReg dest, shiftOperand src)
            | MIR.RawAlloc (dest, numBytes) ->
                MIR.RawAlloc (shiftReg dest, shiftOperand numBytes)
            | MIR.MappedAlloc (dest, numBytes) ->
                MIR.MappedAlloc (shiftReg dest, shiftOperand numBytes)
            | MIR.RawFree ptr -> MIR.RawFree (shiftOperand ptr)
            | MIR.MappedFree ptr -> MIR.MappedFree (shiftOperand ptr)
            | MIR.RawGet (dest, ptr, byteOffset, valueType) ->
                MIR.RawGet (
                    shiftReg dest,
                    shiftOperand ptr,
                    shiftOperand byteOffset,
                    valueType)
            | MIR.RawGetByte (dest, ptr, byteOffset) ->
                MIR.RawGetByte (
                    shiftReg dest,
                    shiftOperand ptr,
                    shiftOperand byteOffset)
            | MIR.RawWriteWord (ptr, byteOffset, value) ->
                MIR.RawWriteWord (
                    shiftOperand ptr,
                    shiftOperand byteOffset,
                    shiftOperand value)
            | MIR.RawWriteByte (ptr, byteOffset, value) ->
                MIR.RawWriteByte (
                    shiftOperand ptr,
                    shiftOperand byteOffset,
                    shiftOperand value)
            | MIR.RawSlotInit (ptr, byteOffset, value, valueType) ->
                MIR.RawSlotInit (
                    shiftOperand ptr,
                    shiftOperand byteOffset,
                    shiftOperand value,
                    valueType)
            | MIR.StringToRawPtr (dest, value) ->
                MIR.StringToRawPtr (shiftReg dest, shiftOperand value)
            | MIR.RawPtrToString (dest, ptr) ->
                MIR.RawPtrToString (shiftReg dest, shiftOperand ptr)
            | MIR.BlobToRawPtr (dest, value) ->
                MIR.BlobToRawPtr (shiftReg dest, shiftOperand value)
            | MIR.RawPtrToBlob (dest, ptr) ->
                MIR.RawPtrToBlob (shiftReg dest, shiftOperand ptr)
            | MIR.DictToRawPtr (dest, dict) ->
                MIR.DictToRawPtr (shiftReg dest, shiftOperand dict)
            | MIR.RawPtrToDict (dest, ptr, tag) ->
                MIR.RawPtrToDict (
                    shiftReg dest,
                    shiftOperand ptr,
                    shiftOperand tag)
            | MIR.ListToRawPtr (dest, list) ->
                MIR.ListToRawPtr (shiftReg dest, shiftOperand list)
            | MIR.RawPtrToList (dest, ptr, tag) ->
                MIR.RawPtrToList (
                    shiftReg dest,
                    shiftOperand ptr,
                    shiftOperand tag)
            | MIR.RefCountIncString value ->
                MIR.RefCountIncString (shiftOperand value)
            | MIR.RefCountDecString value ->
                MIR.RefCountDecString (shiftOperand value)
            | MIR.RefCountIncBlob value ->
                MIR.RefCountIncBlob (shiftOperand value)
            | MIR.RefCountDecBlob value ->
                MIR.RefCountDecBlob (shiftOperand value)
            | MIR.RandomInt64 dest -> MIR.RandomInt64 (shiftReg dest)
            | MIR.DateTimeNow dest -> MIR.DateTimeNow (shiftReg dest)
            | MIR.Sleep (effectId, dest, delayMs) ->
                MIR.Sleep (
                    shiftEffectId effectId,
                    shiftReg dest,
                    shiftOperand delayMs)
            | MIR.CliNative (dest, operation, args) ->
                MIR.CliNative (shiftReg dest, operation, shiftOperands args)
            | MIR.FloatToString (dest, value) ->
                MIR.FloatToString (shiftReg dest, shiftOperand value)
            | MIR.Phi (dest, sources, valueType) ->
                MIR.Phi (
                    shiftReg dest,
                    sources
                    |> List.map (fun (operand, label) ->
                        (shiftOperand operand, label)),
                    valueType)
            | MIR.CoverageHit exprId -> MIR.CoverageHit exprId
        let shiftTerminator = function
            | MIR.Ret value -> MIR.Ret (shiftOperand value)
            | MIR.Branch (condition, trueLabel, falseLabel) ->
                MIR.Branch (
                    shiftOperand condition,
                    trueLabel,
                    falseLabel)
            | MIR.Jump label -> MIR.Jump label
        let blocks =
            func.CFG.Blocks
            |> Map.map (fun _ block ->
                {
                    block with
                        Instrs = block.Instrs |> List.map shiftInstr
                        Terminator = shiftTerminator block.Terminator
                })
        {
            func with
                TypedParams =
                    func.TypedParams
                    |> List.map (fun param ->
                        { param with Reg = shiftReg param.Reg })
                CFG = { func.CFG with Blocks = blocks }
                FloatRegs = func.FloatRegs |> Set.map (fun id -> id - offset)
        }

type internal LirFunctionReferenceComparer() =
    interface IEqualityComparer<LIR.Function> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(func) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(func)

type internal MirFunctionNameHashComparer() =
    interface IEqualityComparer<MIR.Function> with
        member _.Equals(left, right) = left = right
        member _.GetHashCode(func) = StringComparer.Ordinal.GetHashCode(func.Name)

[<NoComparison>]
type internal AllocatedLirFunctionKey = {
    Arch: Platform.Arch
    Function: LIR.Function
}

type internal AllocatedLirFunctionKeyNameHashComparer() =
    interface IEqualityComparer<AllocatedLirFunctionKey> with
        member _.Equals(left, right) =
            left.Arch = right.Arch && left.Function = right.Function
        member _.GetHashCode(key) =
            StringComparer.Ordinal.GetHashCode(key.Function.Name)

type internal ObjectReferenceComparer() =
    interface IEqualityComparer<obj> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(value) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(value)

[<NoComparison>]
type internal AnfDependencyKey = {
    Functions: CheckedAST.FunctionDef list
    LocalRegistries: AST_to_ANF.Registries
    NonInlineableFunctionNames: Set<string>
}

type internal AnfDependencyKeyNameHashComparer() =
    interface IEqualityComparer<AnfDependencyKey> with
        member _.Equals(left, right) = left = right
        member _.GetHashCode(key) =
            let addName hash name =
                (hash * 397) ^^^ StringComparer.Ordinal.GetHashCode(name)
            let functionHash =
                key.Functions
                |> List.fold (fun hash func -> addName hash func.Name) 17
            key.NonInlineableFunctionNames
            |> Set.fold addName functionHash

[<NoComparison>]
type internal CompiledDependencyConfig = {
    Target: Platform.Target
    Options: CompilerOptions
    NonInlineableFunctionNames: Set<string>
}

[<NoComparison>]
type internal StartCompilationConfig = {
    Target: Platform.Target
    Options: CompilerOptions
    BoundaryProgramType: AST.Type
}

type internal SsaFunctionCache =
    MIR.Function -> (unit -> MIR.Function) -> MIR.Function

[<NoComparison>]
type MirOptimizationKey = {
    Function: MIR.Function
    Options: MIROptimizationFacts.OptimizeOptions
    EffectFreeCalls: Set<string>
}

type internal MirOptimizationKeyNameHashComparer() =
    interface IEqualityComparer<MirOptimizationKey> with
        member _.Equals(left, right) =
            left.Function = right.Function
            && left.Options = right.Options
            && left.EffectFreeCalls = right.EffectFreeCalls
        member _.GetHashCode(key) =
            StringComparer.Ordinal.GetHashCode(key.Function.Name)

type internal MirOptimizationCache =
    MirOptimizationKey -> (unit -> MIR.Function) -> MIR.Function

type internal AllocatedLirFunctionCache =
    Platform.Arch -> LIR.Function -> (unit -> LIR.Function) -> LIR.Function

type internal FunctionCompilationCaches = {
    ConvertSsa: SsaFunctionCache
    OptimizeMir: MirOptimizationCache
    AllocateLir: AllocatedLirFunctionCache
}

type internal Arm64InstructionChunkReferenceComparer() =
    interface IEqualityComparer<ARM64Symbolic.Instr list> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(instructions) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(instructions)

type internal Arm64InstructionChunkGroupReferenceComparer() =
    interface IEqualityComparer<ARM64Symbolic.Instr list list> with
        member _.Equals(left, right) = Object.ReferenceEquals(left, right)
        member _.GetHashCode(instructionParts) =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(instructionParts)

[<NoComparison>]
type internal Arm64MetadataGroupKey = {
    Functions: LIR.Function list
}

type internal Arm64MetadataGroupKeyComparer() =
    interface IEqualityComparer<Arm64MetadataGroupKey> with
        member _.Equals(left, right) =
            List.length left.Functions = List.length right.Functions
            && List.forall2
                (fun leftFunction rightFunction ->
                    Object.ReferenceEquals(leftFunction, rightFunction))
                left.Functions
                right.Functions
        member _.GetHashCode(key) =
            key.Functions
            |> List.fold
                (fun hash func ->
                    (hash * 397)
                    ^^^ System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(func))
                17

[<NoComparison>]
type internal Arm64HelperCacheKey = {
    Target: ARM64.TargetConfig
    Options: ARM64CodeGenTypes.CodeGenOptions
    Helper: CodeGen.HelperCacheKey
}

let private mergeMirRegistryOverlay baseRegistry overlay =
    Map.fold (fun merged name value -> Map.add name value merged) baseRegistry overlay

let internal projectMirRegistryOverlay
    ((baseVariants, baseRecords): MIR.VariantRegistry * MIR.RecordRegistry)
    (localVariantLookup: LoweringPrimitives.VariantLookup)
    (localRecordFields: Map<string, (string * AST.Type) list>)
    : MIR.VariantRegistry * MIR.RecordRegistry =
    let localVariants = ANF_to_MIR.buildVariantRegistry localVariantLookup
    let localRecords = ANF_to_MIR.buildRecordRegistry localRecordFields
    (mergeMirRegistryOverlay baseVariants localVariants,
     mergeMirRegistryOverlay baseRecords localRecords)
