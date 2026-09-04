// 7_X86_64_Resolve.fs - x86-64 Label Resolution and Fixup
//
// Resolves symbolic labels (CALL, JMP, Jcc, LEA_rip) into concrete
// relative offsets. Uses a two-pass approach:
//
// Pass 1: Encode all instructions to get their byte sizes, record
//         label positions and fixup locations.
// Pass 2: Patch rel32 fields with correct relative offsets.
//
// x86-64 relative branches are offset from the END of the instruction
// (i.e., the address of the next instruction), not the start.

module X86_64_Resolve

open X86_64

/// A fixup records where a rel32 placeholder needs to be patched
type Fixup = {
    /// Byte offset in the output where the rel32 starts
    PatchOffset: int
    /// Byte offset of the instruction AFTER this one (where PC will be when executing)
    NextInstrOffset: int
    /// The label name this fixup targets
    TargetLabel: string
}

/// Encode a list of x86-64 instructions with label resolution.
/// Result of resolving and encoding
type ResolveResult = {
    MachineCode: byte array
    LabelPositions: Map<string, int>
    /// Fixups deferred for data labels resolved after code size is known
    DeferredFixups: Fixup list
}

/// Require a code label position when downstream binary layout depends on it.
let requireLabelPosition (label: string) (labelPositions: Map<string, int>) : Result<int, string> =
    match Map.tryFind label labelPositions with
    | Some offset -> Ok offset
    | None -> Error $"Missing required label: {label}"

/// Patch a signed rel32 displacement into already-encoded machine code.
let patchRel32 (machineCode: byte array) (patchOffset: int) (rel: int) : unit =
    let relBytes = [|
        byte (uint32 rel &&& 0xFFu)
        byte ((uint32 rel >>> 8) &&& 0xFFu)
        byte ((uint32 rel >>> 16) &&& 0xFFu)
        byte ((uint32 rel >>> 24) &&& 0xFFu)
    |]
    machineCode.[patchOffset] <- relBytes.[0]
    machineCode.[patchOffset + 1] <- relBytes.[1]
    machineCode.[patchOffset + 2] <- relBytes.[2]
    machineCode.[patchOffset + 3] <- relBytes.[3]

type private EncodeState = {
    LabelPositions: Map<string, int>
    Fixups: Fixup list
    Offset: int
    EncodedChunks: byte array list
}

let private addFixup
    (state: EncodeState)
    (instr: Instr)
    (patchOffsetFromInstrStart: int)
    (targetLabel: string)
    : EncodeState =
    let bytes = X86_64_Encoding.encodeInstruction instr
    { state with
        Fixups =
            { PatchOffset = state.Offset + patchOffsetFromInstrStart
              NextInstrOffset = state.Offset + bytes.Length
              TargetLabel = targetLabel } :: state.Fixups
        Offset = state.Offset + bytes.Length
        EncodedChunks = bytes :: state.EncodedChunks }

let private addEncodedInstruction (state: EncodeState) (instr: Instr) : EncodeState =
    let bytes = X86_64_Encoding.encodeInstruction instr
    { state with
        Offset = state.Offset + bytes.Length
        EncodedChunks = bytes :: state.EncodedChunks }

let private encodeInstruction (state: EncodeState) (instr: Instr) : Result<EncodeState, string> =
    match instr with
    | Label name ->
        if Map.containsKey name state.LabelPositions then
            Error $"Duplicate label: {name}"
        else
            Ok { state with LabelPositions = Map.add name state.Offset state.LabelPositions }
    | CALL label ->
        Ok (addFixup state instr 1 label)
    | JMP label ->
        Ok (addFixup state instr 1 label)
    | Jcc (_, label) ->
        Ok (addFixup state instr 2 label)
    | LEA_rip (_, label) ->
        Ok (addFixup state instr 3 label)
    | _ ->
        Ok (addEncodedInstruction state instr)

type private PatchState = { Errors: string list }

/// Collect every symbolic string reference in first-use order. The empty string
/// is always first because runtime helpers also address that canonical buffer.
let collectStringPool (instructions: Instr list) : LiteralPool.StringPool =
    let (_, initial) = LiteralPool.addString LiteralPool.emptyStringPool ""
    instructions
    |> List.fold (fun pool instruction ->
        match instruction with
        | LEA_rip (_, label) ->
            match X86_64.tryStringLiteralValue label with
            | Some value -> LiteralPool.addString pool value |> snd
            | None -> pool
        | _ -> pool) initial

let private stringEntrySize (length: int) : int =
    8 + ((length + 7) &&& (~~~7)) + 8

/// Resolve symbolic literal/runtime labels against the data segment layout used
/// by Binary_Generation_ELF_X86_64.
let dataLabelOffsets
    (codeFileOffset: int)
    (codeSize: int)
    (stringPool: LiteralPool.StringPool)
    : Map<string, int> =
    let dataStart = (codeFileOffset + codeSize + 7) &&& (~~~7)
    let (dataEnd, literalLabels) =
        stringPool.Strings
        |> Map.toList
        |> List.sortBy fst
        |> List.fold (fun (offset, labels) (_, (value, length)) ->
            let labels = Map.add (X86_64.stringLiteralLabel value) offset labels
            (offset + stringEntrySize length, labels)) (dataStart, Map.empty)
    let emptyOffset =
        Map.tryFind (X86_64.stringLiteralLabel "") literalLabels
        |> Option.defaultValue dataStart
    literalLabels
    |> Map.add "_empty_dynamic_buffer" emptyOffset
    |> Map.add "_leak_count" dataEnd

/// Returns the final machine code bytes and label positions.
let resolveAndEncode (instructions: Instr list) : Result<ResolveResult, string> =
    // Pass 1: encode all instructions, collect label positions and fixups
    let encodeResult =
        instructions
        |> List.fold
            (fun state instr -> state |> Result.bind (fun state -> encodeInstruction state instr))
            (Ok
                { LabelPositions = Map.empty
                  Fixups = []
                  Offset = 0
                  EncodedChunks = [] })

    match encodeResult with
    | Error err -> Error err
    | Ok encodeState ->
        // Concatenate all encoded chunks
        let result = encodeState.EncodedChunks |> List.rev |> Array.concat

        // Pass 2: apply fixups (defer unknown labels for data label patching later)
        let deferred =
            encodeState.Fixups
            |> List.fold
                (fun deferred fixup ->
                    match Map.tryFind fixup.TargetLabel encodeState.LabelPositions with
                    | None ->
                        fixup :: deferred
                    | Some targetOffset ->
                        // rel32 = target - nextInstr
                        let rel = targetOffset - fixup.NextInstrOffset
                        patchRel32 result fixup.PatchOffset rel
                        deferred)
                []

        Ok
            { MachineCode = result
              LabelPositions = encodeState.LabelPositions
              DeferredFixups = List.rev deferred }

let private patchDataLabel
    (dataLabels: Map<string, int>)
    (codeFileOffset: int)
    (machineCode: byte array)
    (state: PatchState)
    (fixup: Fixup)
    : PatchState =
    match Map.tryFind fixup.TargetLabel dataLabels with
    | None ->
        { state with Errors = $"Undefined label: {fixup.TargetLabel}" :: state.Errors }
    | Some fileOffset ->
        let targetCodeOffset = fileOffset - codeFileOffset
        let rel = targetCodeOffset - fixup.NextInstrOffset
        patchRel32 machineCode fixup.PatchOffset rel
        state

/// Patch deferred fixups with data label positions.
/// dataLabels maps label names to file offsets. codeFileOffset is where code starts in the file.
let patchDataLabels (result: ResolveResult) (dataLabels: Map<string, int>) (codeFileOffset: int) : Result<ResolveResult, string> =
    let patchState =
        result.DeferredFixups
        |> List.fold
            (patchDataLabel dataLabels codeFileOffset result.MachineCode)
            { Errors = [] }

    if patchState.Errors.IsEmpty then
        Ok { result with DeferredFixups = [] }
    else
        Error (String.concat "\n" (List.rev patchState.Errors))
