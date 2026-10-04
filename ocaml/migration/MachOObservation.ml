(* Compare complete Mach-O images with identical UUID entropy supplied before generation. *)
[@@@warning "-4"]
open Dark_compiler
module M=ControlledMachO
module B=X64EncodingObservation
let tuple values=`Assoc ["tuple",`List values]
let list f xs=`List (List.map f xs)
let attempt f action=try SemanticJson.union "FSharpResult" "Ok" [f (action ())] with Failure msg | Invalid_argument msg -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string msg]
let observe source =
 let stringPools=List.map (fun values -> LiteralPool.createStringPool (List.to_seq values)) [[];[""];[source;"é";"a";"1234567";"12345678";"123456789";source];["😀";"é";"";source]] in
 let floatPools=List.map (fun values -> LiteralPool.createFloatPool (List.to_seq values)) [[];[0.;-0.;1.;infinity;Int64.float_of_bits 0x7ff8000000000001L;-0.];[1.;2.;1.]] in
 let codeCases=List.map (fun length -> Array.init length (fun n -> Int32.logxor 0xd65f03c0l (Int32.of_int (n*1024)))) [0;1;2;3;7;17;3870;3890;3900;4096] in
 let literals=list (fun sp -> list (fun fp -> let data,labels=M.createStringData sp in tuple [B.bytes data;`Assoc ["map",list (fun (key,value) -> tuple [SemanticJson.string key;SemanticJson.int32 value]) (StringOrder.Map.bindings labels)];B.bytes (M.createFloatData fp)]) floatPools) stringPools in
 let images=list (fun words -> tuple [attempt B.bytes (fun () -> M.createExecutable words);list (fun sp -> tuple [attempt B.bytes (fun () -> M.createExecutableWithStrings words sp);list (fun fp -> list (fun leak -> tuple [attempt B.bytes (fun () -> M.createExecutableWithPools words sp fp leak);list (fun count -> attempt B.bytes (fun () -> M.createExecutableWithCoverage words sp fp count leak)) [0;1;9]]) [false;true]) floatPools]) stringPools]) codeCases in
 let pads=list (fun s -> list (fun n -> attempt B.bytes (fun () -> M.padString s n)) [-10;0;1;2;3;4;5;15;16;32]) [source;"é";"😀";"abcdefghijklmnopqrstuv";HostText.ofUtf16Units [|0xd800;97;0xdc00|]] in
 let primitive=tuple [list (fun v -> B.bytes (M.uint32ToBytes v)) [0l;1l;Int32.min_int;Int32.max_int;-1l];list (fun v -> B.bytes (M.uint64ToBytes v)) [0L;1L;Int64.min_int;Int64.max_int;-1L]] in
 let serializers=list (fun value -> let word=Int64.to_int32 value in
 let machHeader:Binary.machHeader={Binary.magic=word;cpuType=word;cpuSubType=word;fileType=word;numCommands=word;sizeOfCommands=word;flags=word;reserved=word} in
 let section64:Binary.section64={Binary.sectionName=source;segmentName=source;address=value;size=value;offset=word;align=word;relocationOffset=word;numRelocations=word;flags=word;reserved1=word;reserved2=word;reserved3=word} in
 let segmentCommand64:Binary.segmentCommand64={Binary.command=word;commandSize=word;segmentName=source;vmAddress=value;vmSize=value;fileOffset=value;fileSize=value;maxProt=word;initProt=word;numSections=word;flags=word;sections=[section64;section64]} in
 let mainCommand:Binary.mainCommand={Binary.command=word;commandSize=word;entryOffset=value;stackSize=value} in
 let dylinkerCommand:Binary.dylinkerCommand={Binary.command=word;commandSize=32l;name=source} in
 let dylibCommand:Binary.dylibCommand={Binary.command=word;commandSize=56l;name=source;timestamp=word;currentVersion=word;compatibilityVersion=word} in
 let uuidCommand:Binary.uuidCommand={Binary.command=word;commandSize=word;uuid=Bytes.init 16 (fun n -> Char.chr n)} in
 let buildVersionCommand:Binary.buildVersionCommand={Binary.command=word;commandSize=word;platform=word;minOS=word;sdk=word;numTools=word} in
 let symtabCommand:Binary.symtabCommand={Binary.command=word;commandSize=word;symbolTableOffset=word;numSymbols=word;stringTableOffset=word;stringTableSize=word} in
 let dysymtabCommand:Binary.dysymtabCommand={Binary.command=word;commandSize=word;localSymIndex=word;numLocalSymbols=word;extDefSymIndex=word;numExtDefSymbols=word;undefSymIndex=word;numUndefSymbols=word;tocOffset=word;numTocEntries=word;modTableOffset=word;numModTableEntries=word;extRefSymOffset=word;numExtRefSyms=word;indirectSymOffset=word;numIndirectSyms=word;extRelOffset=word;numExtRel=word;locRelOffset=word;numLocRel=word} in
 tuple [B.bytes (M.serializeMachHeader machHeader);B.bytes (M.serializeSection64 section64);B.bytes (M.serializeSegmentCommand64 segmentCommand64);B.bytes (M.serializeMainCommand mainCommand);B.bytes (M.serializeDylinkerCommand dylinkerCommand);B.bytes (M.serializeDylibCommand dylibCommand);B.bytes (M.serializeUuidCommand uuidCommand);B.bytes (M.serializeBuildVersionCommand buildVersionCommand);B.bytes (M.serializeSymtabCommand symtabCommand);B.bytes (M.serializeDysymtabCommand dysymtabCommand)]) [0L;1L;Int64.min_int;Int64.max_int;-1L] in
 let first=Binary_Generation_MachO.createExecutable [|0xd65f03c0l|] in
 let second=Binary_Generation_MachO.createExecutable [|0xd65f03c0l|] in
 let rec uuidOffset offset=let command=Bytes.get_int32_le first offset in if command=Binary.lc_UUID then offset+8 else uuidOffset (offset+Int32.to_int (Bytes.get_int32_le first (offset+4))) in
 let offset=uuidOffset 32 in
 let valid bytes=(Char.code (Bytes.get bytes (offset+7)) land 240)=64 && (Char.code (Bytes.get bytes (offset+8)) land 192)=128 in
 let sameOtherBytes=ref true in for i=0 to Bytes.length first-1 do if (i<offset || i>=offset+16) && Bytes.get first i<>Bytes.get second i then sameOtherBytes:=false done;
 let entropy=tuple [`Bool (valid first);`Bool (valid second);`Bool (Bytes.sub first offset 16<>Bytes.sub second offset 16);`Bool !sameOtherBytes] in
 tuple [literals;images;pads;primitive;serializers;entropy]
