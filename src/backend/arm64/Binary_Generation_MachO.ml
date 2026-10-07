(*
   Binary_Generation_MachO.fs - Mach-O Binary Generation (Pass 8, macOS variant)
   Generates a complete Mach-O executable from ARM64 machine code for macOS.
   This is a direct binary generator - no assembler or linker needed.
   File structure:
   [Mach Header]        - Magic, CPU type, flags
   [Load Commands]      - Describe segments, entry point, libraries
   [Padding]            - Space for codesign to add LC_CODE_SIGNATURE
   [__text section]     - Machine code
   [__const section]    - Float pool + string pool (8-byte aligned)
   Load commands created:
   - LC_SEGMENT_64 __PAGEZERO: 4GB unmapped for null pointer protection
   - LC_SEGMENT_64 __TEXT: Code and constant data
   - LC_SEGMENT_64 __LINKEDIT: Symbol tables (empty)
   - LC_DYLINKER: Path to /usr/lib/dyld
   - LC_LOAD_DYLIB: libSystem.B.dylib reference
   - LC_SYMTAB/LC_DYSYMTAB: Symbol tables (empty but required)
   - LC_UUID: Unique binary identifier
   - LC_BUILD_VERSION: macOS version requirement
   - LC_MAIN: Entry point offset into __TEXT
   Code signing: Required for macOS execution, done via `codesign -s -` (ad-hoc).
   See docs/compiler/backend/binary-generation.md for detailed documentation.
*)
[@@@warning "-4"]
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b=Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
let mul a b=Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
let utf8Bytes = Bytes.of_string
(*
   Helper: Pad string to fixed size with null bytes
*)
let padString s size =
 let bytes=utf8Bytes s in
 if Bytes.length bytes>size then Bytes.sub bytes 0 (max 0 size)
 else Bytes.cat bytes (Bytes.make (sub size (Bytes.length bytes)) '\000')
(*
   Helper: Convert uint32 to little-endian bytes
*)
let uint32ToBytes value=Bytes.init 4 (fun i -> Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical value (i*8)) 0xffl)))
(*
   Helper: Convert uint64 to little-endian bytes
*)
let uint64ToBytes value=Bytes.init 8 (fun i -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical value (i*8)) 0xffL)))
let writeUInt64LittleEndian bytes offset value =
 for i=0 to 7 do Bytes.set bytes (add offset i) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical value (i*8)) 0xffL))) done
let align8Int value=mul ((add value 7)/8) 8
(*
   Convert machine code words to little-endian bytes without per-word arrays
*)
let machineCodeToBytes machineCode =
 let byteCount=mul (Array.length machineCode) 4 in
 Bytes.init byteCount (fun i -> let word=machineCode.(i/4) in let shift=(i mod 4)*8 in Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical word shift) 0xffl)))
(*
   Serialize MachHeader to bytes
*)
let serializeMachHeader (header:Binary.machHeader)=Bytes.concat Bytes.empty [uint32ToBytes header.Binary.magic;uint32ToBytes header.Binary.cpuType;uint32ToBytes header.Binary.cpuSubType;uint32ToBytes header.Binary.fileType;uint32ToBytes header.Binary.numCommands;uint32ToBytes header.Binary.sizeOfCommands;uint32ToBytes header.Binary.flags;uint32ToBytes header.Binary.reserved]
(*
   Serialize Section64 to bytes
*)
let serializeSection64 (section:Binary.section64)=Bytes.concat Bytes.empty [padString section.Binary.sectionName 16;padString section.Binary.segmentName 16;uint64ToBytes section.Binary.address;uint64ToBytes section.Binary.size;uint32ToBytes section.Binary.offset;uint32ToBytes section.Binary.align;uint32ToBytes section.Binary.relocationOffset;uint32ToBytes section.Binary.numRelocations;uint32ToBytes section.Binary.flags;uint32ToBytes section.Binary.reserved1;uint32ToBytes section.Binary.reserved2;uint32ToBytes section.Binary.reserved3]
(*
   Serialize SegmentCommand64 to bytes
*)
let serializeSegmentCommand64 (segment:Binary.segmentCommand64)=Bytes.concat Bytes.empty ([uint32ToBytes segment.Binary.command;uint32ToBytes segment.Binary.commandSize;padString segment.Binary.segmentName 16;uint64ToBytes segment.Binary.vmAddress;uint64ToBytes segment.Binary.vmSize;uint64ToBytes segment.Binary.fileOffset;uint64ToBytes segment.Binary.fileSize;uint32ToBytes segment.Binary.maxProt;uint32ToBytes segment.Binary.initProt;uint32ToBytes segment.Binary.numSections;uint32ToBytes segment.Binary.flags]@List.map serializeSection64 segment.Binary.sections)
(*
   Serialize MainCommand to bytes
*)
let serializeMainCommand (main:Binary.mainCommand)=Bytes.concat Bytes.empty [uint32ToBytes main.Binary.command;uint32ToBytes main.Binary.commandSize;uint64ToBytes main.Binary.entryOffset;uint64ToBytes main.Binary.stackSize]
(*
   Serialize DylinkerCommand to bytes
   Offset to name string (after command + cmdsize + offset)
*)
let serializeDylinkerCommand (dylinker:Binary.dylinkerCommand)=Bytes.concat Bytes.empty [uint32ToBytes dylinker.Binary.command;uint32ToBytes dylinker.Binary.commandSize;uint32ToBytes 12l;padString dylinker.Binary.name (sub (Int32.to_int dylinker.Binary.commandSize) 12)]
(*
   Serialize DylibCommand to bytes
   Offset to name string (after command + cmdsize + offset + timestamp + current_version + compatibility_version)
*)
let serializeDylibCommand (dylib:Binary.dylibCommand)=Bytes.concat Bytes.empty [uint32ToBytes dylib.Binary.command;uint32ToBytes dylib.Binary.commandSize;uint32ToBytes 24l;uint32ToBytes dylib.Binary.timestamp;uint32ToBytes dylib.Binary.currentVersion;uint32ToBytes dylib.Binary.compatibilityVersion;padString dylib.Binary.name (sub (Int32.to_int dylib.Binary.commandSize) 24)]
(*
   Serialize UuidCommand to bytes
*)
let serializeUuidCommand (uuid:Binary.uuidCommand)=Bytes.concat Bytes.empty [uint32ToBytes uuid.Binary.command;uint32ToBytes uuid.Binary.commandSize;uuid.Binary.uuid]
(*
   Serialize BuildVersionCommand to bytes
*)
let serializeBuildVersionCommand (buildVer:Binary.buildVersionCommand)=Bytes.concat Bytes.empty [uint32ToBytes buildVer.Binary.command;uint32ToBytes buildVer.Binary.commandSize;uint32ToBytes buildVer.Binary.platform;uint32ToBytes buildVer.Binary.minOS;uint32ToBytes buildVer.Binary.sdk;uint32ToBytes buildVer.Binary.numTools]
(*
   Serialize SymtabCommand to bytes
*)
let serializeSymtabCommand (symtab:Binary.symtabCommand)=Bytes.concat Bytes.empty [uint32ToBytes symtab.Binary.command;uint32ToBytes symtab.Binary.commandSize;uint32ToBytes symtab.Binary.symbolTableOffset;uint32ToBytes symtab.Binary.numSymbols;uint32ToBytes symtab.Binary.stringTableOffset;uint32ToBytes symtab.Binary.stringTableSize]
(*
   Serialize DysymtabCommand to bytes
*)
let serializeDysymtabCommand (dysymtab:Binary.dysymtabCommand)=Bytes.concat Bytes.empty [uint32ToBytes dysymtab.Binary.command;uint32ToBytes dysymtab.Binary.commandSize;uint32ToBytes dysymtab.Binary.localSymIndex;uint32ToBytes dysymtab.Binary.numLocalSymbols;uint32ToBytes dysymtab.Binary.extDefSymIndex;uint32ToBytes dysymtab.Binary.numExtDefSymbols;uint32ToBytes dysymtab.Binary.undefSymIndex;uint32ToBytes dysymtab.Binary.numUndefSymbols;uint32ToBytes dysymtab.Binary.tocOffset;uint32ToBytes dysymtab.Binary.numTocEntries;uint32ToBytes dysymtab.Binary.modTableOffset;uint32ToBytes dysymtab.Binary.numModTableEntries;uint32ToBytes dysymtab.Binary.extRefSymOffset;uint32ToBytes dysymtab.Binary.numExtRefSyms;uint32ToBytes dysymtab.Binary.indirectSymOffset;uint32ToBytes dysymtab.Binary.numIndirectSyms;uint32ToBytes dysymtab.Binary.extRelOffset;uint32ToBytes dysymtab.Binary.numExtRel;uint32ToBytes dysymtab.Binary.locRelOffset;uint32ToBytes dysymtab.Binary.numLocRel]
(*
   Calculate the size of load commands
*)
let calculateCommandsSize (binary:Binary.machOBinary) =
 List.fold_left Int32.add 0l [binary.Binary.pageZeroCommand.Binary.commandSize;binary.Binary.textSegmentCommand.Binary.commandSize;binary.Binary.linkeditSegmentCommand.Binary.commandSize;binary.Binary.dylinkerCommand.Binary.commandSize;binary.Binary.dylibCommand.Binary.commandSize;binary.Binary.symtabCommand.Binary.commandSize;binary.Binary.dysymtabCommand.Binary.commandSize;binary.Binary.uuidCommand.Binary.commandSize;binary.Binary.buildVersionCommand.Binary.commandSize;binary.Binary.mainCommand.Binary.commandSize]
(*
   Serialize complete Mach-O binary to bytes
   sizeof(mach_header_64)
   Pad to code offset (extract from first section's offset)
   Pad to fill the entire __TEXT segment
   Account for 8-byte alignment padding between code and data (for float alignment)
   Align to 8 bytes before float/string data
*)
let serializeMachO (binary:Binary.machOBinary) =
 let headerSize=32 in let commandsSize=Int32.to_int (calculateCommandsSize binary) in let dataStart=add headerSize commandsSize in
 let codeOffset=match binary.Binary.textSegmentCommand.Binary.sections with section::_ -> Int32.to_int section.Binary.offset | [] -> Crash.crash "MachO: TextSegmentCommand has no sections" in
 let paddingBeforeCode=sub codeOffset dataStart in
 if paddingBeforeCode<0 then Crash.crash (Printf.sprintf "MachO: code offset %d is before end of load commands %d" codeOffset dataStart);
 let paddingBefore=Bytes.make paddingBeforeCode '\000' in
 let textSegmentSize=Int64.to_int32 binary.Binary.textSegmentCommand.Binary.fileSize |> Int32.to_int in
 let codeSize=Bytes.length binary.Binary.machineCode in let alignedDataStart=(add (add codeOffset codeSize) 7) land (lnot 7) in
 let alignmentPadding=Bytes.make (sub (sub alignedDataStart codeOffset) codeSize) '\000' in
 let stringSize=Bytes.length binary.Binary.stringData in let paddingAfterCode=sub (sub textSegmentSize alignedDataStart) stringSize in
 if paddingAfterCode<0 then Crash.crash (Printf.sprintf "MachO: __TEXT file size %d is too small for code and data ending at %d" textSegmentSize (add alignedDataStart stringSize));
 let paddingAfter=Bytes.make paddingAfterCode '\000' in
 Bytes.concat Bytes.empty [serializeMachHeader binary.Binary.header;serializeSegmentCommand64 binary.Binary.pageZeroCommand;serializeSegmentCommand64 binary.Binary.textSegmentCommand;serializeSegmentCommand64 binary.Binary.linkeditSegmentCommand;serializeDylinkerCommand binary.Binary.dylinkerCommand;serializeDylibCommand binary.Binary.dylibCommand;serializeSymtabCommand binary.Binary.symtabCommand;serializeDysymtabCommand binary.Binary.dysymtabCommand;serializeUuidCommand binary.Binary.uuidCommand;serializeBuildVersionCommand binary.Binary.buildVersionCommand;serializeMainCommand binary.Binary.mainCommand;paddingBefore;binary.Binary.machineCode;alignmentPadding;binary.Binary.stringData;paddingAfter]
(*
   Create float data bytes from float pool
   Returns byte array of 8-byte IEEE 754 doubles
   Array order is the first-use literal index.
*)
let createFloatData (floatPool:LiteralPool.floatPool)=
 let bytes=Bytes.make (mul (Array.length floatPool.LiteralPool.floats) 8) '\000' in
 Array.iteri (fun index floatVal -> writeUInt64LittleEndian bytes (mul index 8) (Int64.bits_of_float floatVal)) floatPool.LiteralPool.floats;bytes
(*
   Create string data bytes and label map from string pool
   Format: [refcount:8][length:8][data:N][padding:P] for each string
   Returns (string bytes, label map from "_strN" to offset within string section)
   Array.zeroCreate supplies the alignment padding; entries are in ID order.
   Match label format in CodeGen
*)
let createStringData (stringPool:LiteralPool.stringPool)=
 let totalSize=Array.fold_left (fun size (_,len) -> add (add size 16) (align8Int len)) 0 stringPool.LiteralPool.strings in
 let bytes=Bytes.make totalSize '\000' in
 let _,labelMap=Array.fold_left (fun (offset,labels) (idx,(str,len)) ->
 let alignedLen=align8Int len in writeUInt64LittleEndian bytes offset 0x7fffffffffffffffL;
 writeUInt64LittleEndian bytes (add offset 8) (Int64.of_int len);
 let utf8=utf8Bytes str in Bytes.blit utf8 0 bytes (add offset 16) (Bytes.length utf8);
 let label="str_"^string_of_int idx in add (add offset 16) alignedLen,StringOrder.Map.add label offset labels)
 (0,StringOrder.Map.empty) (Array.mapi (fun idx value -> idx,value) stringPool.LiteralPool.strings) in bytes,labelMap
let newUuid = Uuidm.v4_gen (Random.State.make_self_init ())
let newUuidBytes () = Bytes.of_string (Uuidm.to_binary_string (newUuid ()))
(*
   Create a Mach-O executable with already-laid-out constant data.
   VM addresses - load at typical location
   Command sizes (needed for CommandSize fields)
   If we have constant data (floats or strings), we need 2 sections (__text and __const)
   24 bytes for fixed fields + 32 bytes for padded library path
   Place code right after headers and load commands
   Add extra space (200 bytes) for codesign to add LC_CODE_SIGNATURE and other modifications
   Round up to 8-byte alignment
   Constant data (floats + strings) is serialized after 8-byte padding.
   __PAGEZERO segment (required by modern macOS)
   2^2 = 4 byte alignment
   __const section for constant data (floats and strings)
   2^3 = 8 byte alignment (for float alignment)
   Regular section for read-only data
   Use a page-aligned segment size (16KB like ld produces)
   __LINKEDIT segment (required for code signing - will be populated by codesign)
   Will be populated by codesign
   LC_SYMTAB command (empty symbol table - codesign may populate)
   LC_DYSYMTAB command (empty dynamic symbol table)
   LC_LOAD_DYLINKER command
   LC_LOAD_DYLIB command (link to libSystem)
   Standard value (ignored by dyld)
   0.0.0
   1.0.0
   LC_UUID command - generate unique UUID for each binary
   LC_BUILD_VERSION command
   1 = macOS
   macOS 11.0 (Big Sur)
   SDK 15.5
   No tool entries
   __PAGEZERO, __TEXT, __LINKEDIT, DYLINKER, DYLIB, SYMTAB, DYSYMTAB, UUID, BUILD_VERSION, MAIN
*)
let createExecutableWithDataBytes machineCode dataBytes enableLeakCheck =
 let codeBytes=machineCodeToBytes machineCode in
 let hasData=Bytes.length dataBytes>0 in
 let codeSize=Int64.of_int (Bytes.length codeBytes) in let dataSize=Int64.of_int (Bytes.length dataBytes) in
 let vmBase=0x100000000L in
 let pageZeroCommandSize=72 in let numTextSections=if hasData then 2 else 1 in
 let textSegmentCommandSize=add 72 (mul 80 numTextSections) in
 let linkeditSegmentCommandSize=72 in let dylinkerCommandSize=32 in let dylibCommandSize=56 in let symtabCommandSize=24 in let dysymtabCommandSize=80 in let uuidCommandSize=24 in let buildVersionCommandSize=24 in let mainCommandSize=24 in
 let commandsSize=List.fold_left add 0 [pageZeroCommandSize;textSegmentCommandSize;linkeditSegmentCommandSize;dylinkerCommandSize;dylibCommandSize;symtabCommandSize;dysymtabCommandSize;uuidCommandSize;buildVersionCommandSize;mainCommandSize] in
 let headerSize=32 in let codeFileOffset=Int64.of_int ((add (add (add headerSize commandsSize) 200) 7) land (lnot 7)) in let vmCodeOffset=codeFileOffset in
 let dataFileOffset=Int64.logand (Int64.add (Int64.add codeFileOffset codeSize) 7L) (Int64.lognot 7L) in
 let vmDataOffset=Int64.logand (Int64.add (Int64.add vmCodeOffset codeSize) 7L) (Int64.lognot 7L) in
 let pageZeroCommand:Binary.segmentCommand64={Binary.command=Binary.lc_SEGMENT_64;commandSize=Int32.of_int pageZeroCommandSize;segmentName="__PAGEZERO";vmAddress=0L;vmSize=vmBase;fileOffset=0L;fileSize=0L;maxProt=0l;initProt=0l;numSections=0l;flags=0l;sections=[]} in
 let textSection:Binary.section64={Binary.sectionName="__text";segmentName="__TEXT";address=Int64.add vmBase vmCodeOffset;size=codeSize;offset=Int64.to_int32 codeFileOffset;align=2l;relocationOffset=0l;numRelocations=0l;flags=Int32.logor (Int32.logor (Binary.s_REGULAR) (Binary.s_ATTR_PURE_INSTRUCTIONS)) (Binary.s_ATTR_SOME_INSTRUCTIONS);reserved1=0l;reserved2=0l;reserved3=0l} in
 let constSection:Binary.section64={Binary.sectionName="__const";segmentName="__TEXT";address=Int64.add vmBase vmDataOffset;size=dataSize;offset=Int64.to_int32 dataFileOffset;align=3l;relocationOffset=0l;numRelocations=0l;flags=Binary.s_REGULAR;reserved1=0l;reserved2=0l;reserved3=0l} in
 (* Include headers, code and constants, then align to the ARM64 16 KiB page. *)
 let textEnd=Int64.add dataFileOffset dataSize in
 let textSegmentSize=Int64.logand (Int64.add textEnd 0x3fffL) (Int64.lognot 0x3fffL) in
 let textSections=if hasData then [textSection;constSection] else [textSection] in
 let textSegmentProt=if enableLeakCheck then Int32.logor (Int32.logor Binary.vm_PROT_READ Binary.vm_PROT_WRITE) Binary.vm_PROT_EXECUTE else Int32.logor Binary.vm_PROT_READ Binary.vm_PROT_EXECUTE in
 let textSegmentCommand:Binary.segmentCommand64={Binary.command=Binary.lc_SEGMENT_64;commandSize=Int32.of_int textSegmentCommandSize;segmentName="__TEXT";vmAddress=vmBase;vmSize=textSegmentSize;fileOffset=0L;fileSize=textSegmentSize;maxProt=textSegmentProt;initProt=textSegmentProt;numSections=Int32.of_int numTextSections;flags=0l;sections=textSections} in
 let linkeditVmAddress=Int64.add vmBase textSegmentSize in let linkeditFileOffset=textSegmentSize in
 let linkeditSegmentCommand:Binary.segmentCommand64={Binary.command=Binary.lc_SEGMENT_64;commandSize=Int32.of_int linkeditSegmentCommandSize;segmentName="__LINKEDIT";vmAddress=linkeditVmAddress;vmSize=0L;fileOffset=linkeditFileOffset;fileSize=0L;maxProt=Binary.vm_PROT_READ;initProt=Binary.vm_PROT_READ;numSections=0l;flags=0l;sections=[]} in
 let symtabCommand:Binary.symtabCommand={Binary.command=Binary.lc_SYMTAB;commandSize=Int32.of_int symtabCommandSize;symbolTableOffset=0l;numSymbols=0l;stringTableOffset=0l;stringTableSize=0l} in
 let dysymtabCommand:Binary.dysymtabCommand={Binary.command=Binary.lc_DYSYMTAB;commandSize=Int32.of_int dysymtabCommandSize;localSymIndex=0l;numLocalSymbols=0l;extDefSymIndex=0l;numExtDefSymbols=0l;undefSymIndex=0l;numUndefSymbols=0l;tocOffset=0l;numTocEntries=0l;modTableOffset=0l;numModTableEntries=0l;extRefSymOffset=0l;numExtRefSyms=0l;indirectSymOffset=0l;numIndirectSyms=0l;extRelOffset=0l;numExtRel=0l;locRelOffset=0l;numLocRel=0l} in
 let dylinkerCommand:Binary.dylinkerCommand={Binary.command=Binary.lc_LOAD_DYLINKER;commandSize=Int32.of_int dylinkerCommandSize;name="/usr/lib/dyld"} in
 let dylibCommand:Binary.dylibCommand={Binary.command=Binary.lc_LOAD_DYLIB;commandSize=Int32.of_int dylibCommandSize;name="/usr/lib/libSystem.B.dylib";timestamp=2l;currentVersion=0x00000000l;compatibilityVersion=0x00010000l} in
 let uuidCommand:Binary.uuidCommand={Binary.command=Binary.lc_UUID;commandSize=Int32.of_int uuidCommandSize;uuid=newUuidBytes ()} in
 let buildVersionCommand:Binary.buildVersionCommand={Binary.command=Binary.lc_BUILD_VERSION;commandSize=Int32.of_int buildVersionCommandSize;platform=1l;minOS=0xB0000l;sdk=0xF0500l;numTools=0l} in
 let mainCommand:Binary.mainCommand={Binary.command=Binary.lc_MAIN;commandSize=Int32.of_int mainCommandSize;entryOffset=codeFileOffset;stackSize=0L} in
 let header:Binary.machHeader={Binary.magic=Binary.mh_MAGIC_64;cpuType=Binary.cpu_TYPE_ARM64;cpuSubType=Binary.cpu_SUBTYPE_ARM64_ALL;fileType=Binary.mh_EXECUTE;numCommands=10l;sizeOfCommands=Int32.of_int commandsSize;flags=Int32.logor (Int32.logor (Int32.logor (Binary.mh_NOUNDEFS) (Binary.mh_DYLDLINK)) (Binary.mh_TWOLEVEL)) (Binary.mh_PIE);reserved=0l} in
 let binary:Binary.machOBinary={Binary.header=header;pageZeroCommand=pageZeroCommand;textSegmentCommand=textSegmentCommand;linkeditSegmentCommand=linkeditSegmentCommand;dylinkerCommand=dylinkerCommand;dylibCommand=dylibCommand;symtabCommand=symtabCommand;dysymtabCommand=dysymtabCommand;uuidCommand=uuidCommand;buildVersionCommand=buildVersionCommand;mainCommand=mainCommand;machineCode=codeBytes;stringData=dataBytes} in
 serializeMachO binary
(*
   Create a Mach-O executable with float and string data
   Create float data (goes after code, before strings)
   Create string data
*)
let createExecutableWithPools machineCode stringPool floatPool enableLeakCheck =
 let floatBytes=createFloatData floatPool in let stringBytes,_stringLabelMap=createStringData stringPool in
 let floatAndStringBytes=Bytes.cat floatBytes stringBytes in
 let leakBytes=if enableLeakCheck then Bytes.make 8 '\000' else Bytes.empty in
 let leakStart=align8Int (Bytes.length floatAndStringBytes) in let leakPadding=Bytes.make (sub leakStart (Bytes.length floatAndStringBytes)) '\000' in
 let dataBytes=if enableLeakCheck then Bytes.concat Bytes.empty [floatAndStringBytes;leakPadding;leakBytes] else floatAndStringBytes in
 createExecutableWithDataBytes machineCode dataBytes enableLeakCheck
(*
   Create a Mach-O executable with string data (legacy wrapper for backwards compatibility)
*)
let createExecutableWithStrings machineCode stringPool=createExecutableWithPools machineCode stringPool LiteralPool.emptyFloatPool false
(*
   Create a minimal Mach-O executable from ARM64 machine code (legacy, no data)
*)
let createExecutable machineCode=createExecutableWithPools machineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false
(*
   Create a Mach-O executable with coverage data section
   For now, coverage data is included in the __const section
   TODO: Add proper __DATA segment for writable coverage data on macOS
   For now, just include the coverage as zeros in the data section
   This won't actually work for writing on macOS (need __DATA segment)
   but allows the binary to be generated
   Create float data
   Create string data
   Create coverage data (zeros)
   This is a simplified approach - proper coverage on macOS needs __DATA segment
*)
let createExecutableWithCoverage machineCode stringPool floatPool coverageExprCount enableLeakCheck =
 let _codeBytes=machineCodeToBytes machineCode in
 let floatBytes=createFloatData floatPool in let stringBytes,_=createStringData stringPool in
 let coverageSize=align8Int (mul coverageExprCount 8) in let coverageBytes=Bytes.make coverageSize '\000' in
 let floatAndStringBytes=Bytes.cat floatBytes stringBytes in let alignedCoverageStart=align8Int (Bytes.length floatAndStringBytes) in
 let coveragePadding=Bytes.make (sub alignedCoverageStart (Bytes.length floatAndStringBytes)) '\000' in
 let afterCoverage=add alignedCoverageStart (Bytes.length coverageBytes) in
 let leakBytes=if enableLeakCheck then Bytes.make 8 '\000' else Bytes.empty in let leakStart=align8Int afterCoverage in let leakPadding=Bytes.make (sub leakStart afterCoverage) '\000' in
 let allDataBytes=Bytes.concat Bytes.empty (if enableLeakCheck then [floatAndStringBytes;coveragePadding;coverageBytes;leakPadding;leakBytes] else [floatAndStringBytes;coveragePadding;coverageBytes]) in
 createExecutableWithDataBytes machineCode allDataBytes enableLeakCheck
let ioMessage = function Unix.Unix_error (error,_,_) -> Unix.error_message error | Sys_error message -> message | ex -> Printexc.to_string ex
let tryWriteAllBytes path bytes =
 try Out_channel.with_open_bin path (fun output -> Out_channel.output_bytes output bytes);Ok ()
 with ex -> Error (Printf.sprintf "Failed to write Mach-O executable to %s: %s" path (Printexc.to_string ex))
let tryAddUserExecute path =
 try let permissions=(Unix.stat path).Unix.st_perm in Unix.chmod path (permissions lor 0o100);Ok ()
 with ex -> Error (Printf.sprintf "Failed to make Mach-O executable %s: %s" path (Printexc.to_string ex))
let tryStartCodesign path =
 let stdoutRead,stdoutWrite=Unix.pipe ~cloexec:true () in let stderrRead,stderrWrite=Unix.pipe ~cloexec:true () in
 try let process=Unix.create_process "codesign" [|"codesign";"-s";"-";path|] Unix.stdin stdoutWrite stderrWrite in
 Unix.close stdoutWrite;Unix.close stderrWrite;Ok (process,stdoutRead,stderrRead)
 with ex -> List.iter Unix.close [stdoutRead;stdoutWrite;stderrRead;stderrWrite];Error ("Failed to start codesign: "^ioMessage ex)
(*
   Write bytes to file and sign it
   Code sign with adhoc signature (required for macOS to execute)
*)
let writeToFile path bytes =
 match tryWriteAllBytes path bytes with Error err -> Error err | Ok () ->
 match tryAddUserExecute path with Error err -> Error err | Ok () ->
 match tryStartCodesign path with Error err -> Error err | Ok (process,stdoutRead,stderrRead) ->
 Fun.protect ~finally:(fun () -> Unix.close stdoutRead;Unix.close stderrRead) (fun () ->
 let _,status=Unix.waitpid [] process in
 match status with Unix.WEXITED 0 -> Ok () | _ ->
 let input=Unix.in_channel_of_descr (Unix.dup stderrRead) in
 let stderr=Fun.protect ~finally:(fun () -> close_in input) (fun () -> In_channel.input_all input) in
 Error ("Code signing failed: "^stderr))
