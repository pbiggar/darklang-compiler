(*
   Binary_Generation_ELF.fs - ELF Binary Generation (Pass 8, Linux variant)
   Generates a complete ELF executable from ARM64 machine code for Linux.
   This is a direct binary generator - no assembler or linker needed.
   File structure:
   [ELF Header]         - Magic (0x7F 'ELF'), architecture, entry point
   [Program Header]     - Describes loadable segment (PT_LOAD)
   [Machine Code]       - ARM64 instructions
   [Constant Data]      - Float pool + string pool (8-byte aligned)
   Memory layout:
   - Base address: 0x400000 (traditional ELF user-space address)
   - Single PT_LOAD segment: headers + code + data
   - Flags: PF_R | PF_X (readable and executable)
   No code signing needed on Linux (unlike macOS).
   See docs/compiler/backend/binary-generation.md for detailed documentation.
*)
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b=Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
let mul a b=Int32.to_int (Int32.mul (Int32.of_int a) (Int32.of_int b))
(*
   Helper: Convert uint16 to little-endian bytes
*)
let uint16ToBytes value=Bytes.init 2 (fun i -> Char.chr ((value lsr (i*8)) land 255))
(*
   Helper: Convert uint32 to little-endian bytes
*)
let uint32ToBytes value=Bytes.init 4 (fun i -> Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical value (i*8)) 0xffl)))
(*
   Helper: Convert uint64 to little-endian bytes
*)
let uint64ToBytes value=Bytes.init 8 (fun i -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical value (i*8)) 0xffL)))
(*
   Convert machine code words to little-endian bytes without per-word arrays
*)
let machineCodeToBytes machineCode =
 let bytes=Bytes.make (mul (Array.length machineCode) 4) '\000' in
 Array.iteri (fun index word -> let offset=mul index 4 in
 Bytes.set bytes offset (Char.chr (Int32.to_int (Int32.logand word 0xffl)));
 Bytes.set bytes (add offset 1) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical word 8) 0xffl)));
 Bytes.set bytes (add offset 2) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical word 16) 0xffl)));
 Bytes.set bytes (add offset 3) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical word 24) 0xffl)))) machineCode;bytes
let align8Int value=mul ((add value 7)/8) 8
let align8UInt64 value=Int64.logand (Int64.add value 7L) (Int64.lognot 7L)
let writeUInt64LittleEndian bytes offset value =
 for i=0 to 7 do Bytes.set bytes (add offset i) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical value (i*8)) 0xffL))) done
(*
   Serialize ELF64 header to bytes
   16 bytes
*)
let serializeElf64Header (header:ELF.elf64Header)=Bytes.concat Bytes.empty [header.ELF.ident;uint16ToBytes header.ELF.typ;uint16ToBytes header.ELF.machine;uint32ToBytes header.ELF.version;uint64ToBytes header.ELF.entry;uint64ToBytes header.ELF.phOff;uint64ToBytes header.ELF.shOff;uint32ToBytes header.ELF.flags;uint16ToBytes header.ELF.ehSize;uint16ToBytes header.ELF.phEntSize;uint16ToBytes header.ELF.phNum;uint16ToBytes header.ELF.shEntSize;uint16ToBytes header.ELF.shNum;uint16ToBytes header.ELF.shStrNdx]
(*
   Serialize ELF64 program header to bytes
*)
let serializeElf64ProgramHeader (ph:ELF.elf64ProgramHeader)=Bytes.concat Bytes.empty [uint32ToBytes ph.ELF.typ;uint32ToBytes ph.ELF.flags;uint64ToBytes ph.ELF.offset;uint64ToBytes ph.ELF.vAddr;uint64ToBytes ph.ELF.pAddr;uint64ToBytes ph.ELF.fileSize;uint64ToBytes ph.ELF.memSize;uint64ToBytes ph.ELF.align]
(*
   Serialize complete ELF binary to bytes
   Adds alignment padding between code and data for 8-byte alignment
   Calculate alignment padding needed after code
   Align to 8 bytes before float/string data
*)
let serializeElf (binary:ELF.elfBinary)=
 let headerSize=add 64 (mul 56 (List.length binary.ELF.programHeaders)) in let codeEnd=add headerSize (Bytes.length binary.ELF.machineCode) in
 let alignedDataStart=align8Int codeEnd in let alignmentPadding=Bytes.make (sub alignedDataStart codeEnd) '\000' in
 Bytes.concat Bytes.empty (serializeElf64Header binary.ELF.header::List.map serializeElf64ProgramHeader binary.ELF.programHeaders@[binary.ELF.machineCode;alignmentPadding;binary.ELF.stringData])
(*
   Create float data bytes from float pool
   Array order is the first-use literal index.
*)
let createFloatData (floatPool:LiteralPool.floatPool)=
 let bytes=Bytes.make (mul (Array.length floatPool.LiteralPool.floats) 8) '\000' in
 Array.iteri (fun index floatVal -> writeUInt64LittleEndian bytes (mul index 8) (Int64.bits_of_float floatVal)) floatPool.LiteralPool.floats;bytes
let utf8Bytes = Bytes.of_string
(*
   Create string data bytes from string pool
   Format: [refcount:8 bytes][length:8 bytes][data:N bytes][padding:P] for each string.
   Literal strings use INT64_MAX in the refcount slot so shared string RC code can skip them.
   Array.zeroCreate supplies the alignment padding; entries are in ID order.
*)
let createStringData (stringPool:LiteralPool.stringPool)=
 let totalSize=Array.fold_left (fun size (_,len) -> add (add size 16) (align8Int len)) 0 stringPool.LiteralPool.strings in
 let bytes=Bytes.make totalSize '\000' in
 let _=Array.fold_left (fun offset (str,len) -> let alignedLen=align8Int len in
 writeUInt64LittleEndian bytes offset 0x7fffffffffffffffL;
 writeUInt64LittleEndian bytes (add offset 8) (Int64.of_int len);
 let utf8=utf8Bytes str in Bytes.blit utf8 0 bytes (add offset 16) (Bytes.length utf8);
 add (add offset 16) alignedLen) 0 stringPool.LiteralPool.strings in bytes
let elfHeaderSize=64L
let programHeaderSize=56L
let numProgramHeaders=1
let baseVAddr=0x400000L
let codeFileOffset=Int64.add elfHeaderSize (Int64.mul (Int64.of_int numProgramHeaders) programHeaderSize)
let createElfHeader entryVAddr : ELF.elf64Header={ELF.ident=ELF.createIdent ();typ=ELF.et_EXEC;machine=ELF.em_AARCH64;version=1l;entry=entryVAddr;phOff=elfHeaderSize;shOff=0L;flags=0l;ehSize=Int64.to_int elfHeaderSize;phEntSize=Int64.to_int programHeaderSize;phNum=numProgramHeaders;shEntSize=0;shNum=0;shStrNdx=0}
let createLoadSegment codeSize dataSize segmentFlags : ELF.elf64ProgramHeader=
 let alignedDataOffset=align8UInt64 (Int64.add codeFileOffset codeSize) in
 let alignmentPadding=Int64.sub alignedDataOffset (Int64.add codeFileOffset codeSize) in
 let segmentFileSize=Int64.add (Int64.add (Int64.add codeFileOffset codeSize) alignmentPadding) dataSize in
 {ELF.typ=ELF.pt_LOAD;flags=segmentFlags;offset=0L;vAddr=baseVAddr;pAddr=baseVAddr;fileSize=segmentFileSize;memSize=segmentFileSize;align=0x1000L}
let codeEntryVAddr=Int64.add baseVAddr codeFileOffset
let executableSegmentFlags enableLeakCheck=if enableLeakCheck then Int32.logor (Int32.logor ELF.pf_R ELF.pf_W) ELF.pf_X else Int32.logor ELF.pf_R ELF.pf_X
let createBinary codeBytes dataBytes segmentFlags : ELF.elfBinary={ELF.header=createElfHeader codeEntryVAddr;programHeaders=[createLoadSegment (Int64.of_int (Bytes.length codeBytes)) (Int64.of_int (Bytes.length dataBytes)) segmentFlags];machineCode=codeBytes;stringData=dataBytes}
(*
   Create an ELF executable with float and string data
   Create float data (goes after code, before strings)
   Create string data
   Write directly into the final ELF image. The old path first allocated a
   byte copy of all machine code, then copied it again while concatenating
   the complete file. Test-heavy compilation repeats that bandwidth for
   the same large runtime on every executable.
   Array.zeroCreate already supplies both alignment padding and the leak
   counter's initial zero value.
*)
let createExecutableWithPools machineCode stringPool floatPool enableLeakCheck =
 let floatBytes=createFloatData floatPool in let stringBytes=createStringData stringPool in
 let codeSize=mul (Array.length machineCode) 4 in let floatAndStringSize=add (Bytes.length floatBytes) (Bytes.length stringBytes) in
 let dataStart=align8Int (add (Int64.to_int codeFileOffset) codeSize) in
 let dataSize=if enableLeakCheck then add (sub (RuntimeDataLayout.elfCounterOffset (add dataStart floatAndStringSize)) dataStart) 8 else floatAndStringSize in
 let programHeader=createLoadSegment (Int64.of_int codeSize) (Int64.of_int dataSize) (executableSegmentFlags enableLeakCheck) in
 let headerBytes=serializeElf64Header (createElfHeader codeEntryVAddr) in let programHeaderBytes=serializeElf64ProgramHeader programHeader in
 let binary=Bytes.make (add dataStart dataSize) '\000' in
 Bytes.blit headerBytes 0 binary 0 (Bytes.length headerBytes);Bytes.blit programHeaderBytes 0 binary (Bytes.length headerBytes) (Bytes.length programHeaderBytes);
 Array.iteri (fun index word -> let offset=add (Int64.to_int codeFileOffset) (mul index 4) in
 for i=0 to 3 do Bytes.set binary (add offset i) (Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical word (i*8)) 0xffl))) done) machineCode;
 Bytes.blit floatBytes 0 binary dataStart (Bytes.length floatBytes);
 Bytes.blit stringBytes 0 binary (add dataStart (Bytes.length floatBytes)) (Bytes.length stringBytes);binary
(*
   Create an ELF executable with string data (legacy wrapper for backwards compatibility)
*)
let createExecutableWithStrings machineCode stringPool=createExecutableWithPools machineCode stringPool LiteralPool.emptyFloatPool false
(*
   Create a minimal ELF executable from ARM64 machine code (legacy, no data)
*)
let createExecutable machineCode=createExecutableWithPools machineCode LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false
(*
   Create an ELF executable with coverage data section
   coverageExprCount: number of coverage expressions (each needs 8 bytes)
   The coverage data is placed after strings and initialized to zero
   Uses a single RWX segment for simplicity (code + data + coverage)
   Create float data (goes after code, before strings)
   Create string data
   Create coverage data (zeros, 8 bytes per expression, 8-byte aligned)
   Single segment: RWX (code + read-only data + coverage data)
   Note: RWX is not ideal for security but simplifies the implementation
*)
let createExecutableWithCoverage machineCode stringPool floatPool coverageExprCount enableLeakCheck =
 let codeBytes=machineCodeToBytes machineCode in let floatBytes=createFloatData floatPool in let stringBytes=createStringData stringPool in
 let coverageSize=align8Int (mul coverageExprCount 8) in let coverageBytes=Bytes.make coverageSize '\000' in
 let floatAndStringBytes=Bytes.cat floatBytes stringBytes in let alignedCoverageStart=align8Int (Bytes.length floatAndStringBytes) in
 let coveragePadding=Bytes.make (sub alignedCoverageStart (Bytes.length floatAndStringBytes)) '\000' in let afterCoverage=add alignedCoverageStart (Bytes.length coverageBytes) in
 let dataBytes=if enableLeakCheck then let leakBytes=Bytes.make 8 '\000' in let dataStart=align8Int (add (Int64.to_int codeFileOffset) (Bytes.length codeBytes)) in
 let leakStart=sub (RuntimeDataLayout.elfCounterOffset (add dataStart afterCoverage)) dataStart in let leakPadding=Bytes.make (sub leakStart afterCoverage) '\000' in
 Bytes.concat Bytes.empty [floatAndStringBytes;coveragePadding;coverageBytes;leakPadding;leakBytes] else Bytes.concat Bytes.empty [floatAndStringBytes;coveragePadding;coverageBytes] in
 serializeElf (createBinary codeBytes dataBytes (Int32.logor (Int32.logor ELF.pf_R ELF.pf_W) ELF.pf_X))
let tryWriteAllBytes path bytes=Result.map_error (fun message->"Failed to write ELF executable to "^path^": "^message) (HostFile.writeBytes path bytes)
let tryAddUserExecute path=try let permissions=(Unix.stat path).Unix.st_perm in Unix.chmod path (permissions lor 0o100);Ok () with exn->Error ("Failed to make ELF executable "^path^": "^HostFile.errorMessage path exn)
(*
   Write bytes to file (Linux - no code signing needed)
*)
let writeToFile path bytes=match tryWriteAllBytes path bytes with Error err -> Error err | Ok () -> tryAddUserExecute path
