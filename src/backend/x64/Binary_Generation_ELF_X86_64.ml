(*
   Binary_Generation_ELF_X86_64.ml - ELF Binary Generation (Pass 8, x64 backend)
   Generates a complete ELF64 executable from x86-64 machine code for Linux.
   Uses the shared Elf64Header / Elf64ProgramHeader types from Binary_ELF.ml.
   Machine code is passed as a byte array because x86-64 has variable-length
   instructions (1-15 bytes each).
*)
let add a b=Int32.to_int (Int32.add (Int32.of_int a) (Int32.of_int b))
let sub a b=Int32.to_int (Int32.sub (Int32.of_int a) (Int32.of_int b))
(*
   Create an x86-64 ELF executable with float and string data.
   entryOffset: byte offset of _start within machineCode (default 0).
   Create float data (goes after code, before strings)
   Create string data
   ELF structures
   Load address - typical for user-space programs
   Code starts right after headers
*)
let createExecutableWithPools machineCode stringPool floatPool enableLeakCheck entryOffset =
 let floatBytes=Backend_Arm64_Binary_Generation_ELF.createFloatData floatPool in
 let stringBytes=Backend_Arm64_Binary_Generation_ELF.createStringData stringPool in
 let dataStart=(add (add 120 (Bytes.length machineCode)) 7) land (lnot 7) in
 let dataBytes=let floatAndStringBytes=Bytes.cat floatBytes stringBytes in
 if enableLeakCheck then let leakBytes=Bytes.make 8 '\000' in let leakStart=sub (RuntimeDataLayout.elfCounterOffset (add dataStart (Bytes.length floatAndStringBytes))) dataStart in
 let leakPadding=Bytes.make (sub leakStart (Bytes.length floatAndStringBytes)) '\000' in Bytes.concat Bytes.empty [floatAndStringBytes;leakPadding;leakBytes] else floatAndStringBytes in
 let codeSize=Int64.of_int (Bytes.length machineCode) in let dataSize=Int64.of_int (Bytes.length dataBytes) in
 let elfHeaderSize=64L in let programHeaderSize=56L in let numProgramHeaders=1 in let baseVAddr=0x400000L in
 let codeFileOffset=Int64.add elfHeaderSize (Int64.mul (Int64.of_int numProgramHeaders) programHeaderSize) in
 let entryVAddr=Int64.add (Int64.add baseVAddr codeFileOffset) (Int64.of_int entryOffset) in
 let header : ELF.elf64Header={ELF.ident=ELF.createIdent ();typ=ELF.et_EXEC;machine=ELF.em_X86_64;version=1l;entry=entryVAddr;phOff=elfHeaderSize;shOff=0L;flags=0l;ehSize=Int64.to_int elfHeaderSize;phEntSize=Int64.to_int programHeaderSize;phNum=numProgramHeaders;shEntSize=0;shNum=0;shStrNdx=0} in
 let alignedDataOffset=Int64.logand (Int64.add (Int64.add codeFileOffset codeSize) 7L) (Int64.lognot 7L) in
 let alignmentPadding=Int64.sub alignedDataOffset (Int64.add codeFileOffset codeSize) in
 let segmentFileSize=Int64.add (Int64.add (Int64.add codeFileOffset codeSize) alignmentPadding) dataSize in let segmentMemSize=segmentFileSize in
 let segmentFlags=if enableLeakCheck then Int32.logor (Int32.logor ELF.pf_R ELF.pf_W) ELF.pf_X else Int32.logor ELF.pf_R ELF.pf_X in
 let codeSegment : ELF.elf64ProgramHeader={ELF.typ=ELF.pt_LOAD;flags=segmentFlags;offset=0L;vAddr=baseVAddr;pAddr=baseVAddr;fileSize=segmentFileSize;memSize=segmentMemSize;align=0x1000L} in
 let binary : ELF.elfBinary={ELF.header;programHeaders=[codeSegment];machineCode;stringData=dataBytes} in
 Backend_Arm64_Binary_Generation_ELF.serializeElf binary
