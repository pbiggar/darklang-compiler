(* Whole runtime instruction selections and their encoded machine words. *)
open Dark_compiler
module J=MachineISAObservation
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let regs=[|ARM64.X0;ARM64.X1;ARM64.X2;ARM64.X3;ARM64.X4;ARM64.X5;ARM64.X6;ARM64.X7;ARM64.X8;ARM64.X9;ARM64.X10;ARM64.X11;ARM64.X12;ARM64.X13;ARM64.X14;ARM64.X15;ARM64.X16;ARM64.X17;ARM64.X18;ARM64.X19;ARM64.X20;ARM64.X21;ARM64.X22;ARM64.X23;ARM64.X24;ARM64.X25;ARM64.X26;ARM64.X27;ARM64.X28;ARM64.X29;ARM64.X30;ARM64.SP|]
let fps=[|ARM64.D0;ARM64.D1;ARM64.D2;ARM64.D3;ARM64.D4;ARM64.D5;ARM64.D6;ARM64.D7;ARM64.D8;ARM64.D9;ARM64.D10;ARM64.D11;ARM64.D12;ARM64.D13;ARM64.D14;ARM64.D15;ARM64.D16;ARM64.D17;ARM64.D18;ARM64.D19;ARM64.D20;ARM64.D21;ARM64.D22;ARM64.D23;ARM64.D24;ARM64.D25;ARM64.D26;ARM64.D27;ARM64.D28;ARM64.D29;ARM64.D30;ARM64.D31|]
let words xs=
 let data=StringOrder.Map.singleton Symbolic.coverageDataLabelName 16384 in
 `List (List.mapi (fun idx instr -> let value=ARM64_Encoding.encodeWithLabels instr (idx*4) StringOrder.Map.empty StringOrder.Map.empty StringOrder.Map.empty data in `Assoc ["kind",`String "uint32";"value",`String (Printf.sprintf "%lu" value)]) xs)
let code xs=tuple [list J.armInstr xs;words xs]
let internalCode f=try tuple [`Bool false;code (f ())] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let observe _source=
 let regsList=Array.to_list regs in
 let chunks=[0;1;0x7fff;0x8000;0xfffe;0xffff] in
 let values=List.concat_map (fun a -> List.concat_map (fun b -> List.concat_map (fun c -> List.map (fun d -> Int64.logor (Int64.logor (Int64.shift_left (Int64.of_int a) 48) (Int64.shift_left (Int64.of_int b) 32)) (Int64.logor (Int64.shift_left (Int64.of_int c) 16) (Int64.of_int d))) chunks) chunks) chunks) chunks in
 let immediates=list (fun reg -> list (fun value -> code (Immediates.generateLoadUInt64Immediate reg value)) values) regsList in
 let signed=list (fun reg -> list (fun value -> internalCode (fun () -> Immediates.generateLoadNonNegativeIntImmediate reg value)) [-2147483648;-1;0;1;65535;65536;2147483647]) regsList in
 let floats=list (fun reg -> list (fun fp -> code (FloatFormatting.generateFloatToString reg fp)) (Array.to_list fps)) regsList in
 let targets=list (fun target ->
  let coverage=list (fun count -> internalCode (fun () -> Coverage.generateCoverageFlush target count)) [-2147483648;-536870912;-268435456;-1;0;1;2;511;512;8191;8192;65535;65536;268435455;268435456;536870911;536870912;2147483647] in
  let hosts=list (fun reg -> tuple [code (HostValues.generateRandomInt64 target reg);code (HostValues.generateDateTimeNow target reg)]) regsList in
  let files=list (fun dest -> list (fun path -> tuple [code (FileRead.generateFileReadBlob target dest path);code (FileMetadata.generateFileExists target dest path);code (FileMetadata.generateFileDelete target dest path);code (FileMetadata.generateFileSetExecutable target dest path)]) regsList) regsList in
  let multi=list (fun role ->
   let dest=regs.(role) in
   let choices=[dest;regs.((role+1) mod 32);ARM64.X19;ARM64.X22] in
   let writes=list (fun path -> list (fun content -> list (fun append -> code (FileWrite.generateFileWriteBlob target dest path content append)) [false;true]) choices) choices in
   let pointers=list (fun path -> list (fun ptr -> list (fun len -> code (WriteFromPointer.generateFileWriteFromPtr target dest path ptr len)) choices) choices) choices in
   tuple [writes;pointers]) (List.init 32 Fun.id) in
  tuple [coverage;hosts;files;multi]) [ARM64.targetConfigFor Platform.MacOSARM64;ARM64.targetConfigFor Platform.LinuxARM64] in
 tuple [immediates;signed;floats;targets]
