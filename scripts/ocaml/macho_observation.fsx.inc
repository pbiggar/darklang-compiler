let machoObservation (source:string) =
    let enc (value:'a)=encode typeof<'a> (box value)
    let tuple values=namedArray "tuple" (Array.ofList values)
    let mapNodes action values=JsonArray(values |> List.map action |> List.toArray) :> JsonNode
    let attempt action =
        let node=JsonObject()
        node["type"] <- JsonValue.Create "FSharpResult"
        let case,fields=try "Ok",[|enc (action ())|] with ex -> "Error",[|enc ex.Message|]
        node["case"] <- JsonValue.Create case
        node["fields"] <- JsonArray(fields)
        node :> JsonNode
    let stringPools=[[];[""];[source;"é";"a";"1234567";"12345678";"123456789";source];["😀";"é";"";source]] |> List.map LiteralPool.createStringPool
    let floatPools=[[];[0.;-0.;1.;infinity;BitConverter.Int64BitsToDouble(0x7ff8000000000001L);-0.];[1.;2.;1.]] |> List.map LiteralPool.createFloatPool
    let codeCases=[0;1;2;3;7;17;3870;3890;3900;4096] |> List.map (fun length -> Array.init length (fun n -> 0xd65f03c0u ^^^ uint32 (n*1024)))
    let literal sp fp =
        let data,labels=ControlledMachO.createStringData sp
        tuple [enc data;enc labels;enc (ControlledMachO.createFloatData fp)]
    let literals=mapNodes (fun sp -> mapNodes (literal sp) floatPools) stringPools
    let pooled words sp fp leak =
        tuple [attempt (fun () -> ControlledMachO.createExecutableWithPools words sp fp leak);mapNodes (fun count -> attempt (fun () -> ControlledMachO.createExecutableWithCoverage words sp fp count leak)) [0;1;9]]
    let stringImage words sp =
        tuple [attempt (fun () -> ControlledMachO.createExecutableWithStrings words sp);mapNodes (fun fp -> mapNodes (pooled words sp fp) [false;true]) floatPools]
    let images=mapNodes (fun words -> tuple [attempt (fun () -> ControlledMachO.createExecutable words);mapNodes (stringImage words) stringPools]) codeCases
    let pads=mapNodes (fun s -> mapNodes (fun n -> attempt (fun () -> ControlledMachO.padString s n)) [-10;0;1;2;3;4;5;15;16;32]) [source;"é";"😀";"abcdefghijklmnopqrstuv";String([|char 0xd800;'a';char 0xdc00|])]
    let primitive=tuple [mapNodes (fun v -> enc (ControlledMachO.uint32ToBytes v)) [0u;1u;0x80000000u;0x7fffffffu;0xffffffffu];mapNodes (fun v -> enc (ControlledMachO.uint64ToBytes v)) [0UL;1UL;0x8000000000000000UL;0x7fffffffffffffffUL;0xffffffffffffffffUL]]
    let serializer value =
        let word=uint32 value
        let machHeader:Binary.MachHeader={Magic=word;CpuType=word;CpuSubType=word;FileType=word;NumCommands=word;SizeOfCommands=word;Flags=word;Reserved=word}
        let section64:Binary.Section64={SectionName=source;SegmentName=source;Address=value;Size=value;Offset=word;Align=word;RelocationOffset=word;NumRelocations=word;Flags=word;Reserved1=word;Reserved2=word;Reserved3=word}
        let segmentCommand64:Binary.SegmentCommand64={Command=word;CommandSize=word;SegmentName=source;VmAddress=value;VmSize=value;FileOffset=value;FileSize=value;MaxProt=word;InitProt=word;NumSections=word;Flags=word;Sections=[section64;section64]}
        let mainCommand:Binary.MainCommand={Command=word;CommandSize=word;EntryOffset=value;StackSize=value}
        let dylinkerCommand:Binary.DylinkerCommand={Command=word;CommandSize=32u;Name=source}
        let dylibCommand:Binary.DylibCommand={Command=word;CommandSize=56u;Name=source;Timestamp=word;CurrentVersion=word;CompatibilityVersion=word}
        let uuidCommand:Binary.UuidCommand={Command=word;CommandSize=word;Uuid=Array.init 16 byte}
        let buildVersionCommand:Binary.BuildVersionCommand={Command=word;CommandSize=word;Platform=word;MinOS=word;Sdk=word;NumTools=word}
        let symtabCommand:Binary.SymtabCommand={Command=word;CommandSize=word;SymbolTableOffset=word;NumSymbols=word;StringTableOffset=word;StringTableSize=word}
        let dysymtabCommand:Binary.DysymtabCommand={Command=word;CommandSize=word;LocalSymIndex=word;NumLocalSymbols=word;ExtDefSymIndex=word;NumExtDefSymbols=word;UndefSymIndex=word;NumUndefSymbols=word;TocOffset=word;NumTocEntries=word;ModTableOffset=word;NumModTableEntries=word;ExtRefSymOffset=word;NumExtRefSyms=word;IndirectSymOffset=word;NumIndirectSyms=word;ExtRelOffset=word;NumExtRel=word;LocRelOffset=word;NumLocRel=word}
        tuple [enc (ControlledMachO.serializeMachHeader machHeader);enc (ControlledMachO.serializeSection64 section64);enc (ControlledMachO.serializeSegmentCommand64 segmentCommand64);enc (ControlledMachO.serializeMainCommand mainCommand);enc (ControlledMachO.serializeDylinkerCommand dylinkerCommand);enc (ControlledMachO.serializeDylibCommand dylibCommand);enc (ControlledMachO.serializeUuidCommand uuidCommand);enc (ControlledMachO.serializeBuildVersionCommand buildVersionCommand);enc (ControlledMachO.serializeSymtabCommand symtabCommand);enc (ControlledMachO.serializeDysymtabCommand dysymtabCommand)]
    let serializers=mapNodes serializer [0UL;1UL;0x8000000000000000UL;0x7fffffffffffffffUL;0xffffffffffffffffUL]
    let first=Binary_Generation_MachO.createExecutable [|0xd65f03c0u|]
    let second=Binary_Generation_MachO.createExecutable [|0xd65f03c0u|]
    let rec uuidOffset offset =
        if BitConverter.ToUInt32(first,offset)=Binary.LC_UUID then offset+8
        else uuidOffset (offset+int (BitConverter.ToUInt32(first,offset+4)))
    let offset=uuidOffset 32
    let valid (bytes:byte array)=(int bytes[offset+7] &&& 240)=64 && (int bytes[offset+8] &&& 192)=128
    let sameOtherBytes=Array.forall (fun i -> (i>=offset && i<offset+16) || first[i]=second[i]) [|0..first.Length-1|]
    let entropy=tuple [enc (valid first);enc (valid second);enc (first[offset..offset+15]<>second[offset..offset+15]);enc sameOtherBytes]
    tuple [literals;images;pads;primitive;serializers;entropy]
