(*
   Binary.ml - Mach-O Binary Format Types
   Defines data structures for the Mach-O binary format used by macOS.
   Mach-O is the executable format for macOS. This module defines the types
   needed to represent Mach-O headers, load commands, segments, and sections.
   Structure of a Mach-O executable:
   - Mach-O Header: Identifies the file type and architecture
   - Load Commands: Instructions for the loader (segments, entry point, etc.)
   - Data: The actual code and data sections
   Our minimal executable has:
   - __PAGEZERO segment (4GB unmapped memory for security)
   - __TEXT segment with __text section (executable code)
   - LC_MAIN command (specifies entry point)
   Mach-O magic number for 64-bit
   CPU type constants
   File type constants
   Executable file
   Flags
   Load command types
   Virtual memory protections
*)
(* Binary.ml - Native executable container data structures. *)
[@@@warning "-30"]
let mh_MAGIC_64 = 0xFEEDFACFl
let cpu_TYPE_ARM64 = 0x0100000Cl
let cpu_SUBTYPE_ARM64_ALL = 0l
let mh_EXECUTE = 0x2l
let mh_NOUNDEFS = 0x1l
let mh_DYLDLINK = 0x4l
let mh_TWOLEVEL = 0x80l
let mh_PIE = 0x200000l
let lc_SEGMENT_64 = 0x19l
let lc_MAIN = 0x80000028l
let lc_LOAD_DYLINKER = 0xEl
let lc_LOAD_DYLIB = 0xCl
let lc_UUID = 0x1Bl
let lc_BUILD_VERSION = 0x32l
let lc_SYMTAB = 0x2l
let lc_DYSYMTAB = 0xBl
let vm_PROT_READ = 0x1l
let vm_PROT_WRITE = 0x2l
let vm_PROT_EXECUTE = 0x4l
(*
   Section flags
*)
let s_REGULAR = 0x0l
let s_ATTR_PURE_INSTRUCTIONS = 0x80000000l
let s_ATTR_SOME_INSTRUCTIONS = 0x00000400l
(*
   Mach-O header
   For 64-bit
*)
type machHeader = {magic : int32; cpuType : int32; cpuSubType : int32; fileType : int32; numCommands : int32; sizeOfCommands : int32; flags : int32; reserved : int32}
(*
   Section within a segment
   Max 16 bytes
*)
type section64 = {sectionName : string; segmentName : string; address : int64; size : int64; offset : int32; align : int32; relocationOffset : int32; numRelocations : int32; flags : int32; reserved1 : int32; reserved2 : int32; reserved3 : int32}
(*
   LC_SEGMENT_64 load command
   Max 16 bytes
*)
type segmentCommand64 = {command : int32; commandSize : int32; segmentName : string; vmAddress : int64; vmSize : int64; fileOffset : int64; fileSize : int64; maxProt : int32; initProt : int32; numSections : int32; flags : int32; sections : section64 list}
(*
   LC_MAIN load command (entry point)
*)
type mainCommand = {command : int32; commandSize : int32; entryOffset : int64; stackSize : int64}
(*
   LC_LOAD_DYLINKER load command
   Path to dylinker (e.g., "/usr/lib/dyld")
*)
type dylinkerCommand = {command : int32; commandSize : int32; name : string}
(*
   LC_LOAD_DYLIB load command
   Path to library (e.g., "/usr/lib/libSystem.B.dylib")
*)
type dylibCommand = {command : int32; commandSize : int32; name : string; timestamp : int32; currentVersion : int32; compatibilityVersion : int32}
(*
   LC_UUID load command
   16 bytes
*)
type uuidCommand = {command : int32; commandSize : int32; uuid : bytes}
(*
   LC_BUILD_VERSION load command
   1 = macOS
   Minimum OS version (e.g., 11.0 = 0xB0000)
   SDK version
   Number of tool entries (0 for simplicity)
*)
type buildVersionCommand = {command : int32; commandSize : int32; platform : int32; minOS : int32; sdk : int32; numTools : int32}
(*
   LC_SYMTAB load command
*)
type symtabCommand = {command : int32; commandSize : int32; symbolTableOffset : int32; numSymbols : int32; stringTableOffset : int32; stringTableSize : int32}
(*
   LC_DYSYMTAB load command
   Simplified - just the basic fields
   Zero out the rest
*)
type dysymtabCommand = {command : int32; commandSize : int32; localSymIndex : int32; numLocalSymbols : int32; extDefSymIndex : int32; numExtDefSymbols : int32; undefSymIndex : int32; numUndefSymbols : int32; tocOffset : int32; numTocEntries : int32; modTableOffset : int32; numModTableEntries : int32; extRefSymOffset : int32; numExtRefSyms : int32; indirectSymOffset : int32; numIndirectSyms : int32; extRelOffset : int32; numExtRel : int32; locRelOffset : int32; numLocRel : int32}
(*
   Complete Mach-O binary structure
   Constant string data (placed after code)
*)
type machOBinary = {header : machHeader; pageZeroCommand : segmentCommand64; textSegmentCommand : segmentCommand64; linkeditSegmentCommand : segmentCommand64; dylinkerCommand : dylinkerCommand; dylibCommand : dylibCommand; symtabCommand : symtabCommand; dysymtabCommand : dysymtabCommand; uuidCommand : uuidCommand; buildVersionCommand : buildVersionCommand; mainCommand : mainCommand; machineCode : bytes; stringData : bytes}
