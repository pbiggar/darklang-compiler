// RuntimeDataLayoutTests.fs - ELF relocation/image agreement for writable counters.

module RuntimeDataLayoutTests

let private checkCounter image offset =
    if offset % 65536 <> 0 then Error "Writable ELF counter shares a code page"
    elif Array.length image <> offset + 8 then Error "Counter relocation disagrees with ELF image extent"
    elif image |> Array.skip offset |> Array.exists (fun value -> value <> 0uy) then
        Error "ELF counter is not initialized to zero"
    else Ok ()

let private checkArm64 stringLength () =
    let strings = LiteralPool.createStringPool [String.replicate stringLength "x"]
    let code = [|0xd65f03c0u|] // RET; only data layout is under test.
    let labels = ARM64_Encoding.computeLeakCounterLabel Platform.Linux 120 4 0 (ARM64_Encoding.getStringPoolSize strings)
    let image = Binary_Generation_ELF.createExecutableWithPools code strings LiteralPool.emptyFloatPool true
    match Map.tryFind ARM64Symbolic.leakCounterLabelName labels with
    | Some offset -> checkCounter image offset
    | None -> Error "ARM64 counter relocation is absent"

let private checkX86 stringLength () =
    let strings = LiteralPool.createStringPool [String.replicate stringLength "x"]
    let code = [|0xc3uy|] // RET; only data layout is under test.
    let labels = X86_64_Resolve.dataLabelOffsets 120 code.Length strings
    let image = Binary_Generation_ELF_X86_64.createExecutableWithPools code strings LiteralPool.emptyFloatPool true 0
    match Map.tryFind "_leak_count" labels with
    | Some offset -> checkCounter image offset
    | None -> Error "x86 counter relocation is absent"

let tests =
    [0; 4090; 16380; 65530]
    |> List.collect (fun length ->
        [ $"ARM64 counter relocation after {length} string bytes", checkArm64 length
          $"x86 counter relocation after {length} string bytes", checkX86 length ])
