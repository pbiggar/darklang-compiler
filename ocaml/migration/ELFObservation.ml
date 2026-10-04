(* Whole ELF images and their exact serialized structures and literal bytes. *)
[@@@warning "-4"]
open Dark_compiler
module A=Backend_Arm64_Binary_Generation_ELF
module X=Binary_Generation_ELF_X86_64
module B=X64EncodingObservation
let tuple values=`Assoc ["tuple",`List values]
let list f xs=`List (List.map f xs)
let str=SemanticJson.string
let attempt f action=try SemanticJson.union "FSharpResult" "Ok" [f (action ())] with Failure msg | Invalid_argument msg -> SemanticJson.union "FSharpResult" "Error" [str msg]
let observe source =
 let stringPools=List.map (fun values -> LiteralPool.createStringPool (List.to_seq values)) [[];[""];[source;"é";"a";"1234567";"12345678";"123456789";source];["😀";"é";"";source]] in
 let floatPools=List.map (fun values -> LiteralPool.createFloatPool (List.to_seq values)) [[];[0.;-0.;1.;infinity;Int64.float_of_bits 0x7ff8000000000001L;-0.];[1.;2.;1.]] in
 let codeCases=List.map (fun length -> Array.init length (fun n -> Int32.logxor 0xd65f03c0l (Int32.of_int (n*1024)))) [0;1;2;3;4;7;8;17] in
 let literalData=list (fun sp -> list (fun fp -> tuple [ARMEncodingObservation.stringPool sp;attempt B.bytes (fun () -> A.createStringData sp);attempt B.bytes (fun () -> A.createFloatData fp)]) floatPools) stringPools in
 let images=list (fun words -> let machineCode=Bytes.concat Bytes.empty (Array.to_list (Array.map A.uint32ToBytes words)) in
 tuple [B.bytes machineCode;attempt B.bytes (fun () -> A.createExecutable words);list (fun sp -> tuple [attempt B.bytes (fun () -> A.createExecutableWithStrings words sp);list (fun fp -> tuple [attempt B.bytes (fun () -> A.createExecutableWithPools words sp fp false);list (fun entry -> attempt B.bytes (fun () -> X.createExecutableWithPools machineCode sp fp false entry)) [-1;0;1;Int32.to_int Int32.min_int;Int32.to_int Int32.max_int]]) floatPools]) stringPools]) codeCases in
 let instrumented=list (fun words -> let machineCode=Bytes.concat Bytes.empty (Array.to_list (Array.map A.uint32ToBytes words)) in
 list (fun sp -> list (fun fp -> tuple [attempt B.bytes (fun () -> A.createExecutableWithPools words sp fp true);attempt B.bytes (fun () -> X.createExecutableWithPools machineCode sp fp true 0);list (fun count -> list (fun leak -> attempt B.bytes (fun () -> A.createExecutableWithCoverage words sp fp count leak)) [false;true]) [0;1;9]]) [LiteralPool.emptyFloatPool;List.nth floatPools 1]) [LiteralPool.emptyStringPool;List.nth stringPools 2]) [List.nth codeCases 0;List.nth codeCases 3] in
 let x64Alignment=list (fun length -> let code=Bytes.init length (fun i -> Char.chr ((i*23) land 255)) in list (fun sp -> list (fun fp -> attempt B.bytes (fun () -> X.createExecutableWithPools code sp fp false 3)) floatPools) stringPools) (List.init 17 Fun.id) in
 let primitives=tuple [list (fun value -> B.bytes (A.uint16ToBytes value)) [0;1;255;256;65535];list (fun value -> B.bytes (A.uint32ToBytes value)) [0l;1l;Int32.min_int;Int32.max_int;-1l];list (fun value -> B.bytes (A.uint64ToBytes value)) [0L;1L;Int64.min_int;Int64.max_int;-1L]] in
 let headerCases=list (fun value -> let h : ELF.elf64Header={ELF.ident=ELF.createIdent ();typ=65535;machine=ELF.em_AARCH64;version=Int64.to_int32 value;entry=value;phOff=value;shOff=value;flags=Int64.to_int32 value;ehSize=64;phEntSize=56;phNum=1;shEntSize=0;shNum=0;shStrNdx=0} in
 let ph : ELF.elf64ProgramHeader={ELF.typ=Int64.to_int32 value;flags=Int64.to_int32 value;offset=value;vAddr=value;pAddr=value;fileSize=value;memSize=value;align=value} in
 tuple [B.bytes (A.serializeElf64Header h);B.bytes (A.serializeElf64ProgramHeader ph);list (fun count -> list (fun codeSize -> let b : ELF.elfBinary={ELF.header=h;programHeaders=List.init count (fun _ -> ph);machineCode=Bytes.init codeSize (fun n -> Char.chr (n land 255));stringData=Bytes.of_string "raw-data"} in B.bytes (A.serializeElf b)) [0;1;7;8;15]) [0;1;2;3]]) [0L;1L;Int64.min_int;Int64.max_int;-1L] in
 tuple [literalData;images;instrumented;x64Alignment;primitives;headerCases]
