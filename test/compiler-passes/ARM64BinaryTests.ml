(*
   ARM64BinaryTests.fs - Unit and execution tests for ARM64 Mach-O and ELF generation.
*)
[@@@warning "-4-42"]
open Dark_compiler
open ARM64
open Binary_Generation_MachO
(*
   Test result type
*)
type testResult=(unit,string) result
let testUint32ToBytes ()=if uint32ToBytes 0x12345678l<>Bytes.of_string "\x78\x56\x34\x12" then Error "uint32ToBytes failed" else Ok ()
let testUint64ToBytes ()=if uint64ToBytes 0x123456789abcdef0L<>Bytes.of_string "\xf0\xde\xbc\x9a\x78\x56\x34\x12" then Error "uint64ToBytes failed" else Ok ()
let testPadString ()=let padded=padString "hello" 10 in if Bytes.length padded<>10 then Error "padString: wrong length" else if Bytes.get padded 0<>'h' || Bytes.get padded 4<>'o' then Error "padString: wrong content" else if Bytes.get padded 5<>'\000' || Bytes.get padded 9<>'\000' then Error "padString: wrong padding" else Ok ()
let testPadStringTruncate ()=let padded=padString "hello world this is long" 5 in if Bytes.length padded<>5 then Error "padString truncate: wrong length" else if Bytes.get padded 0<>'h' || Bytes.get padded 4<>'o' then Error "padString truncate: wrong content" else Ok ()
let sampleHeader : Binary.machHeader={Binary.magic=Binary.mh_MAGIC_64;cpuType=Binary.cpu_TYPE_ARM64;cpuSubType=Binary.cpu_SUBTYPE_ARM64_ALL;fileType=Binary.mh_EXECUTE;numCommands=2l;sizeOfCommands=100l;flags=Binary.mh_NOUNDEFS;reserved=0l}
let testSerializeMachHeaderSize ()=let bytes=serializeMachHeader sampleHeader in if Bytes.length bytes<>32 then Error (Printf.sprintf "serializeMachHeader: expected size 32, got %d" (Bytes.length bytes)) else Ok ()
let testSerializeMachHeaderMagic ()=if Bytes.sub (serializeMachHeader sampleHeader) 0 4<>Bytes.of_string "\xcf\xfa\xed\xfe" then Error "serializeMachHeader: wrong magic bytes" else Ok ()
let sampleSection : Binary.section64={Binary.sectionName="__text";segmentName="__TEXT";address=0L;size=100L;offset=0l;align=2l;relocationOffset=0l;numRelocations=0l;flags=0l;reserved1=0l;reserved2=0l;reserved3=0l}
let testSerializeSection64Size ()=let bytes=serializeSection64 sampleSection in if Bytes.length bytes<>80 then Error (Printf.sprintf "serializeSection64: expected size 80, got %d" (Bytes.length bytes)) else Ok ()
let testCreateExecutableNonEmpty ()=let binary=createExecutable [|0xd65f03c0l|] in if Bytes.length binary=0 then Error "createExecutable: binary is empty" else Ok ()
let testCreateExecutableMagic ()=if Bytes.sub (createExecutable [|0xd65f03c0l|]) 0 4<>Bytes.of_string "\xcf\xfa\xed\xfe" then Error "createExecutable: wrong magic bytes" else Ok ()
let testCreateExecutableContainsCode ()=
 let binary=createExecutable [|0xd65f03c0l|] in let retBytes=Bytes.of_string "\xc0\x03\x5f\xd6" in
 let rec found i=i+4<=Bytes.length binary && (Bytes.sub binary i 4=retBytes || found (i+1)) in
 if not (found 0) then Error "createExecutable: RET instruction not found in binary" else Ok ()
let readUInt32LE=Bytes.get_int32_le
let testMachOConstSectionOffsetPointsToAlignedData ()=
 let stringPool=LiteralPool.createStringPool (List.to_seq ["abc"]) in
 let binary=createExecutableWithPools [|0xd65f03c0l|] stringPool LiteralPool.emptyFloatPool false in
 let textSegmentOffset=32+72 in let firstSectionOffset=textSegmentOffset+72 in let secondSectionOffset=firstSectionOffset+80 in let sectionFileOffsetField=48 in
 let constFileOffset=Int32.to_int (readUInt32LE binary (secondSectionOffset+sectionFileOffsetField)) in
 let expectedRefcount=Bytes.of_string "\xff\xff\xff\xff\xff\xff\xff\x7f" in let actualRefcount=Bytes.sub binary constFileOffset 8 in
 if constFileOffset mod 8<>0 then Error (Printf.sprintf "Expected __const section file offset to be 8-byte aligned, got %d" constFileOffset)
 else if actualRefcount<>expectedRefcount then Error "Expected __const section offset to point at the string refcount sentinel" else Ok ()
let missingPath prefix=let path=Filename.temp_file prefix "" in Sys.remove path; Filename.concat path "out"
let testElfWriteToFileReturnsErrorForInvalidPath ()=
 match Backend_Arm64_Binary_Generation_ELF.writeToFile (missingPath "dark-elf-missing-") (Bytes.make 1 '\000') with
 | Ok () -> Error "Expected invalid output path to return Error" | Error "" -> Error "Expected writeToFile error message to describe failure" | Error _ -> Ok ()
let testCreateExecutableWithCoverageIncludesCoverageSection ()=
 let binary=createExecutableWithCoverage [|0xd65f03c0l|] LiteralPool.emptyStringPool LiteralPool.emptyFloatPool 3 false in
 let textSegmentOffset=32+72 in let textSegmentNumSectionsOffset=64 in let numSections=readUInt32LE binary (textSegmentOffset+textSegmentNumSectionsOffset) in
 if numSections<>2l then Error (Printf.sprintf "Expected coverage binary to include __text and __const sections, got %lu sections" numSections) else
 let firstSectionOffset=textSegmentOffset+72 in let secondSectionOffset=firstSectionOffset+80 in let sectionSizeField=40 in let constSectionSize=Int32.to_int (readUInt32LE binary (secondSectionOffset+sectionSizeField)) in
 if constSectionSize<24 then Error (Printf.sprintf "Expected __const section to include 24 coverage bytes, got %d" constSectionSize) else Ok ()
let minimalMachOBinaryWithTextSection (textSection:Binary.section64) textFileSize : Binary.machOBinary=
 let pageZeroCommandSize=72l in
 let textSegmentCommandSize=152l in
 let linkeditSegmentCommandSize=72l in
 let dylinkerCommandSize=32l in
 let dylibCommandSize=56l in
 let symtabCommandSize=24l in
 let dysymtabCommandSize=80l in
 let uuidCommandSize=24l in
 let buildVersionCommandSize=24l in
 let mainCommandSize=24l in
 let commandsSize=List.fold_left Int32.add 0l [pageZeroCommandSize;textSegmentCommandSize;linkeditSegmentCommandSize;dylinkerCommandSize;dylibCommandSize;symtabCommandSize;dysymtabCommandSize;uuidCommandSize;buildVersionCommandSize;mainCommandSize] in
 {Binary.header={Binary.magic=Binary.mh_MAGIC_64;cpuType=Binary.cpu_TYPE_ARM64;cpuSubType=Binary.cpu_SUBTYPE_ARM64_ALL;fileType=Binary.mh_EXECUTE;numCommands=10l;sizeOfCommands=commandsSize;flags=Binary.mh_NOUNDEFS;reserved=0l};pageZeroCommand={Binary.command=Binary.lc_SEGMENT_64;commandSize=pageZeroCommandSize;segmentName="__PAGEZERO";vmAddress=0L;vmSize=0x100000000L;fileOffset=0L;fileSize=0L;maxProt=0l;initProt=0l;numSections=0l;flags=0l;sections=[]};textSegmentCommand={Binary.command=Binary.lc_SEGMENT_64;commandSize=textSegmentCommandSize;segmentName="__TEXT";vmAddress=0x100000000L;vmSize=textFileSize;fileOffset=0L;fileSize=textFileSize;maxProt=Int32.logor (Binary.vm_PROT_READ) (Binary.vm_PROT_EXECUTE);initProt=Int32.logor (Binary.vm_PROT_READ) (Binary.vm_PROT_EXECUTE);numSections=1l;flags=0l;sections=[textSection]};linkeditSegmentCommand={Binary.command=Binary.lc_SEGMENT_64;commandSize=linkeditSegmentCommandSize;segmentName="__LINKEDIT";vmAddress=0x100004000L;vmSize=0L;fileOffset=textFileSize;fileSize=0L;maxProt=Binary.vm_PROT_READ;initProt=Binary.vm_PROT_READ;numSections=0l;flags=0l;sections=[]};dylinkerCommand={Binary.command=Binary.lc_LOAD_DYLINKER;commandSize=dylinkerCommandSize;name="/usr/lib/dyld"};dylibCommand={Binary.command=Binary.lc_LOAD_DYLIB;commandSize=dylibCommandSize;name="/usr/lib/libSystem.B.dylib";timestamp=2l;currentVersion=0l;compatibilityVersion=0x00010000l};symtabCommand={Binary.command=Binary.lc_SYMTAB;commandSize=symtabCommandSize;symbolTableOffset=0l;numSymbols=0l;stringTableOffset=0l;stringTableSize=0l};dysymtabCommand={Binary.command=Binary.lc_DYSYMTAB;commandSize=dysymtabCommandSize;localSymIndex=0l;numLocalSymbols=0l;extDefSymIndex=0l;numExtDefSymbols=0l;undefSymIndex=0l;numUndefSymbols=0l;tocOffset=0l;numTocEntries=0l;modTableOffset=0l;numModTableEntries=0l;extRefSymOffset=0l;numExtRefSyms=0l;indirectSymOffset=0l;numIndirectSyms=0l;extRelOffset=0l;numExtRel=0l;locRelOffset=0l;numLocRel=0l};uuidCommand={Binary.command=Binary.lc_UUID;commandSize=uuidCommandSize;uuid=Bytes.make 16 '\000'};buildVersionCommand={Binary.command=Binary.lc_BUILD_VERSION;commandSize=buildVersionCommandSize;platform=1l;minOS=0xB0000l;sdk=0xF0500l;numTools=0l};mainCommand={Binary.command=Binary.lc_MAIN;commandSize=mainCommandSize;entryOffset=Int64.logand (Int64.of_int32 textSection.Binary.offset) 0xffffffffL;stackSize=0L};machineCode=Bytes.of_string "\xc0\x03\x5f\xd6";stringData=Bytes.empty}
let minimalTextSectionAtOffset offset : Binary.section64={Binary.sectionName="__text";segmentName="__TEXT";address=0x100000000L;size=4L;offset;align=2l;relocationOffset=0l;numRelocations=0l;flags=Int32.logor (Int32.logor Binary.s_REGULAR Binary.s_ATTR_PURE_INSTRUCTIONS) Binary.s_ATTR_SOME_INSTRUCTIONS;reserved1=0l;reserved2=0l;reserved3=0l}
let expectMachOLayoutCrash expectedMessage binary=
 try ignore (serializeMachO binary);Error (Printf.sprintf "Expected serializeMachO to crash with '%s'" expectedMessage)
 with Failure msg | Invalid_argument msg -> if msg=expectedMessage then Ok () else Error (Printf.sprintf "Expected Mach-O layout error '%s', got '%s'" expectedMessage msg)
let testSerializeMachOReportsInvalidCodeOffset ()=
 let textSection=minimalTextSectionAtOffset 64l in let binary=minimalMachOBinaryWithTextSection textSection 0x4000L in expectMachOLayoutCrash "MachO: code offset 64 is before end of load commands 592" binary
let testSerializeMachOReportsTextSegmentTooSmall ()=
 let textSection=minimalTextSectionAtOffset 0x1000l in let binary=minimalMachOBinaryWithTextSection textSection 0x1000L in expectMachOLayoutCrash "MachO: __TEXT file size 4096 is too small for code and data ending at 4104" binary
(*
   Test the complete pipeline: instructions -> encoding -> binary
   Verify encoding
   Verify binary generation
*)
let testCompleteEncodingPipeline ()=
 let movInstr=MOVZ (X0,42,0) in let retInstr=RET in let machineCode=ARM64_Encoding.encode movInstr@ARM64_Encoding.encode retInstr in
 if List.length machineCode<>2 then Error "Complete pipeline: wrong machine code count" else if List.nth machineCode 0<>0xd2800540l then Error "Complete pipeline: wrong MOVZ encoding" else if List.nth machineCode 1<>0xd65f03c0l then Error "Complete pipeline: wrong RET encoding" else
 let binary=createExecutable (Array.of_list machineCode) in if Bytes.length binary=0 then Error "Complete pipeline: binary is empty" else if Bytes.sub binary 0 4<>Bytes.of_string "\xcf\xfa\xed\xfe" then Error "Complete pipeline: wrong magic bytes" else Ok ()
(*
   Execute a Linux ARM64 ELF directly on Linux ARM64 and through the pinned
   QEMU build on every other supported development host.
*)
let unixSignalNumber signal = Option.value ~default:signal (List.assoc_opt signal [Sys.sighup,1;Sys.sigint,2;Sys.sigquit,3;Sys.sigill,4;Sys.sigabrt,6;Sys.sigfpe,8;Sys.sigkill,9;Sys.sigusr1,10;Sys.sigsegv,11;Sys.sigusr2,12;Sys.sigpipe,13;Sys.sigalrm,14;Sys.sigterm,15;Sys.sigchld,17;Sys.sigcont,18;Sys.sigstop,19;Sys.sigtstp,20;Sys.sigttin,21;Sys.sigttou,22;Sys.sigurg,23;Sys.sigxcpu,24;Sys.sigxfsz,25;Sys.sigvtalrm,26;Sys.sigprof,27])

let testExecuteLinuxElf ()=
 let machineCode=List.concat_map ARM64_Encoding.encode [MOVZ (X0,42,0);MOVZ (X8,Platform.linuxARM64SyscallNumbers.Platform.exit,0);SVC 0] |> Array.of_list in
 let binary=Backend_Arm64_Binary_Generation_ELF.createExecutable machineCode in
 let tempPath=Filename.temp_file "dark-" "" in
 let outcome=try
 Out_channel.with_open_bin tempPath (fun output -> Out_channel.output_bytes output binary);
 let permissions=(Unix.stat tempPath).Unix.st_perm in Unix.chmod tempPath (permissions lor 0o100);
 let command,args=match Platform.detectOS (),Platform.detectArch () with Ok Platform.Linux,Ok Platform.ARM64 -> tempPath,[|tempPath|] | _ -> "/opt/dcb/qemu/qemu-aarch64",[|"/opt/dcb/qemu/qemu-aarch64";tempPath|] in
 let stderrRead,stderrWrite=Unix.pipe ~cloexec:true () in
 let process=try Unix.create_process command args Unix.stdin Unix.stdout stderrWrite with ex -> Unix.close stderrRead;Unix.close stderrWrite;raise ex in
 Unix.close stderrWrite;
 Fun.protect ~finally:(fun () -> Unix.close stderrRead) (fun () ->
 let deadline=Unix.gettimeofday ()+.10. in
 let rec wait ()=match Unix.waitpid [Unix.WNOHANG] process with
 | 0,_ when Unix.gettimeofday ()<deadline -> ignore (Unix.select [] [] [] 0.01);wait ()
 | 0,_ -> Unix.kill process Sys.sigkill;ignore (Unix.waitpid [] process);Error "Timed out executing Linux ARM64 ELF binary"
 | _,status ->
 let exitCode=match status with Unix.WEXITED code -> code | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128+unixSignalNumber signal in
 if exitCode=42 then Ok () else let input=Unix.in_channel_of_descr (Unix.dup stderrRead) in let stderr=Fun.protect ~finally:(fun () -> close_in input) (fun () -> In_channel.input_all input) in Error (Printf.sprintf "Expected Linux ARM64 ELF exit code 42, got %d: %s" exitCode stderr) in wait ())
 with ex -> Error ("Failed to execute Linux ARM64 ELF binary: "^(match ex with Unix.Unix_error (error,_,_) -> Unix.error_message error | Sys_error message -> message | _ -> Printexc.to_string ex)) in
 (try Sys.remove tempPath with Sys_error _ -> ());outcome
let testWriteToFileReturnsErrorForInvalidPath ()=
 match writeToFile (missingPath "dark-macho-missing-") (Bytes.make 1 '\000') with Ok () -> Error "Expected invalid output path to return Error" | Error _ -> Ok ()
(*
   Literal IDs must agree with the bytes and labels emitted by both formats.
*)
let testLiteralFirstUseLayout ()=
 let stringRef value=Symbolic.DataLabel (Symbolic.StringLiteral value) in let floatRef bits=Symbolic.DataLabel (Symbolic.FloatLiteral (Int64.float_of_bits bits)) in
 let floatBits=[0L;Int64.min_int;0x7ff8000000000001L;0x7ff8000000000002L] in
 let refs=[stringRef "é";stringRef "";stringRef "é";stringRef "z"]@List.map floatRef floatBits@[floatRef Int64.min_int;floatRef 0x7ff8000000000001L] in
 let strings,floats=ARM64_Resolve.collectPoolsFromLabelRefs (List.to_seq refs) in
 let expectedStrings=Bytes.concat Bytes.empty (List.concat_map (fun value -> let bytes=Bytes.of_string value in let padding=(8-Bytes.length bytes mod 8) mod 8 in [uint64ToBytes 0x7fffffffffffffffL;uint64ToBytes (Int64.of_int (Bytes.length bytes));bytes;Bytes.make padding '\000']) ["é";"";"z"]) in
 let expectedFloats=Bytes.concat Bytes.empty (List.map uint64ToBytes floatBits) in let machoStrings,labels=createStringData strings in
 if Backend_Arm64_Binary_Generation_ELF.createStringData strings<>expectedStrings || machoStrings<>expectedStrings then Error "Literal string layout lost first-use order, UTF-8 length, deduplication, or alignment"
 else if not (StringOrder.Map.equal Int.equal labels (StringOrder.Map.of_seq (List.to_seq ["str_0",0;"str_1",24;"str_2",40]))) then Error "Mach-O string labels disagree with literal IDs"
 else if Backend_Arm64_Binary_Generation_ELF.createFloatData floats<>expectedFloats || createFloatData floats<>expectedFloats then Error "Literal float layout lost signed zero, NaN payloads, or deduplication" else Ok ()
(* Linux ELF execution is inapplicable on macOS; generation is still tested. *)
let linuxExecutionTests=match Platform.detectOS () with Ok Platform.MacOS->[] | _->["execute Linux ARM64 ELF",testExecuteLinuxElf]
let tests=["literal first-use layout preserves UTF-8 and exact float bits",testLiteralFirstUseLayout;"uint32ToBytes",testUint32ToBytes;"uint64ToBytes",testUint64ToBytes;"padString",testPadString;"padString truncate",testPadStringTruncate;"serializeMachHeader size",testSerializeMachHeaderSize;"serializeMachHeader magic",testSerializeMachHeaderMagic;"serializeSection64 size",testSerializeSection64Size;"createExecutable non-empty",testCreateExecutableNonEmpty;"createExecutable magic",testCreateExecutableMagic;"createExecutable contains code",testCreateExecutableContainsCode;"Mach-O __const section offset points to aligned data",testMachOConstSectionOffsetPointsToAlignedData;"ELF writeToFile returns Error for invalid path",testElfWriteToFileReturnsErrorForInvalidPath;"createExecutableWithCoverage includes coverage section",testCreateExecutableWithCoverageIncludesCoverageSection;"serializeMachO reports invalid code offset",testSerializeMachOReportsInvalidCodeOffset;"serializeMachO reports undersized __TEXT segment",testSerializeMachOReportsTextSegmentTooSmall;"complete encoding pipeline",testCompleteEncodingPipeline]@linuxExecutionTests@["writeToFile returns Error for invalid path",testWriteToFileReturnsErrorForInvalidPath]
(*
   Run all binary generation unit tests
   Returns Ok () if all pass, Error with first failure message if any fail
*)
let runAll ()=let rec runTests=function [] -> Ok () | (name,test)::rest -> match test () with Ok () -> runTests rest | Error msg -> Error (name^" test failed: "^msg) in runTests tests
