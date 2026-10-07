(* Binary.mli - Native executable container data structures. *)
[@@@warning "-30"]
val mh_MAGIC_64 : int32
val cpu_TYPE_ARM64 : int32
val cpu_SUBTYPE_ARM64_ALL : int32
val mh_EXECUTE : int32
val mh_NOUNDEFS : int32
val mh_DYLDLINK : int32
val mh_TWOLEVEL : int32
val mh_PIE : int32
val lc_SEGMENT_64 : int32
val lc_MAIN : int32
val lc_LOAD_DYLINKER : int32
val lc_LOAD_DYLIB : int32
val lc_UUID : int32
val lc_BUILD_VERSION : int32
val lc_SYMTAB : int32
val lc_DYSYMTAB : int32
val vm_PROT_READ : int32
val vm_PROT_WRITE : int32
val vm_PROT_EXECUTE : int32
val s_REGULAR : int32
val s_ATTR_PURE_INSTRUCTIONS : int32
val s_ATTR_SOME_INSTRUCTIONS : int32
type machHeader = {magic : int32; cpuType : int32; cpuSubType : int32; fileType : int32; numCommands : int32; sizeOfCommands : int32; flags : int32; reserved : int32}
type section64 = {sectionName : string; segmentName : string; address : int64; size : int64; offset : int32; align : int32; relocationOffset : int32; numRelocations : int32; flags : int32; reserved1 : int32; reserved2 : int32; reserved3 : int32}
type segmentCommand64 = {command : int32; commandSize : int32; segmentName : string; vmAddress : int64; vmSize : int64; fileOffset : int64; fileSize : int64; maxProt : int32; initProt : int32; numSections : int32; flags : int32; sections : section64 list}
type mainCommand = {command : int32; commandSize : int32; entryOffset : int64; stackSize : int64}
type dylinkerCommand = {command : int32; commandSize : int32; name : string}
type dylibCommand = {command : int32; commandSize : int32; name : string; timestamp : int32; currentVersion : int32; compatibilityVersion : int32}
type uuidCommand = {command : int32; commandSize : int32; uuid : bytes}
type buildVersionCommand = {command : int32; commandSize : int32; platform : int32; minOS : int32; sdk : int32; numTools : int32}
type symtabCommand = {command : int32; commandSize : int32; symbolTableOffset : int32; numSymbols : int32; stringTableOffset : int32; stringTableSize : int32}
type dysymtabCommand = {command : int32; commandSize : int32; localSymIndex : int32; numLocalSymbols : int32; extDefSymIndex : int32; numExtDefSymbols : int32; undefSymIndex : int32; numUndefSymbols : int32; tocOffset : int32; numTocEntries : int32; modTableOffset : int32; numModTableEntries : int32; extRefSymOffset : int32; numExtRefSyms : int32; indirectSymOffset : int32; numIndirectSyms : int32; extRelOffset : int32; numExtRel : int32; locRelOffset : int32; numLocRel : int32}
type machOBinary = {header : machHeader; pageZeroCommand : segmentCommand64; textSegmentCommand : segmentCommand64; linkeditSegmentCommand : segmentCommand64; dylinkerCommand : dylinkerCommand; dylibCommand : dylibCommand; symtabCommand : symtabCommand; dysymtabCommand : dysymtabCommand; uuidCommand : uuidCommand; buildVersionCommand : buildVersionCommand; mainCommand : mainCommand; machineCode : bytes; stringData : bytes}
