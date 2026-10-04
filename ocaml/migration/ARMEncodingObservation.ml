(* Complete ARM64 word, relocation, chunk and literal-pool observations. *)
[@@@warning "-4"]
open Dark_compiler
module E=ARM64_Encoding
module A=ARM64
module S=Symbolic
module J=MachineISAObservation
let tuple values=`Assoc ["tuple",`List values]
let list f xs=`List (List.map f xs)
let array f xs=list f (Array.to_list xs)
let word value=`Assoc ["kind",`String "uint32";"value",`String (Printf.sprintf "%lu" value)]
let float64 value=`Assoc ["kind",`String "float64";"value",`String (Printf.sprintf "%016Lx" (Int64.bits_of_float value))]
let int64 value=`Assoc ["kind",`String "int64";"value",`String (Int64.to_string value)]
let i=SemanticJson.int32
let str=SemanticJson.string
let map f values=`Assoc ["map",list (fun (key,value) -> tuple [str key;f value]) (StringOrder.Map.bindings values)]
let attempt f action=try SemanticJson.union "FSharpResult" "Ok" [f (action ())] with Failure msg | Invalid_argument msg -> SemanticJson.union "FSharpResult" "Error" [str msg]
let stringPool (p:LiteralPool.stringPool)=SemanticJson.record "StringPool" ["Strings",array (fun (s,len) -> tuple [str s;i len]) p.LiteralPool.strings;"StringToId",map i p.LiteralPool.stringToId]
let floatPool (p:LiteralPool.floatPool)=SemanticJson.record "FloatPool" ["Floats",array float64 p.LiteralPool.floats;"FloatBitsToId",`Assoc ["map",list (fun (k,v) -> tuple [int64 k;i v]) (LiteralPool.FloatBitsMap.bindings p.LiteralPool.floatBitsToId)]]
let pools (s,f)=tuple [stringPool s;floatPool f]
let prepared (p:E.preparedChunk)=SemanticJson.record "PreparedChunk" ["MachineCodeTemplate",array word p.E.machineCodeTemplate;"Relocations",array (fun (idx,instr) -> tuple [i idx;J.symInstr instr]) p.E.relocations;"CodeLabels",array (fun (label,offset) -> tuple [str label;i offset]) p.E.codeLabels;"PoolLabelRefs",array J.symLabelRef p.E.poolLabelRefs]
let observe source=
 let boundaries=[-2147483648;-32768;-520;-512;-256;-1;0;1;2;6;7;8;15;16;24;31;32;48;63;64;255;504;512;4095;4096;32760;32767;65535;2147483647] in
 let instructions=list (fun role -> list (fun boundary -> list (fun instr -> tuple [J.armInstr instr;attempt word (fun () -> E.encodeWord instr);attempt (list word) (fun () -> E.encode instr);attempt prepared (fun () -> E.prepareSymbolicChunk [S.ofARM64 instr])]) (J.armInstructions source role boundary)) boundaries) (List.init 32 Fun.id) in
 let logical=list (fun ones -> list (fun rotation -> let low=Int64.pred (Int64.shift_left 1L ones) in let mask=if rotation=0 then low else Int64.logor (Int64.shift_right_logical low rotation) (Int64.shift_left low (64-rotation)) in list (fun role -> let instr=A.AND_imm ((match role with 0 -> A.X0 | 1 -> A.X16 | _ -> A.SP),A.X30,mask) in tuple [J.armInstr instr;attempt word (fun () -> E.encodeWord instr)]) [0;1;2]) (List.init 64 Fun.id)) (List.init 63 (fun n -> n+1)) in
 let floatValues=[0.;-0.;infinity;neg_infinity;Int64.float_of_bits 0x7ff8000000000001L;0.1]@List.init 256 (fun encoded -> let sign=if encoded land 128=0 then 1. else -1. in let exponent=(encoded lsr 4) land 7 in sign*.(1.+.float_of_int (encoded land 15)/.16.)*.Float.ldexp 1. (if exponent>=4 then exponent-7 else exponent+1)) in
 let fpRegs=[A.D0;A.D7;A.D16;A.D31] in
 let floats=list (fun value -> list (fun reg -> let instr=A.FMOV_imm (reg,value) in tuple [J.armInstr instr;attempt word (fun () -> E.encodeWord instr)]) fpRegs) floatValues in
 let helperRegs=list (fun role -> let instrs=J.armInstructions source role 0 in match instrs with A.MOVZ (reg,_,_)::_ -> tuple [J.armReg reg;word (E.encodeReg reg);word (E.encodeFReg (List.nth [A.D0;A.D1;A.D2;A.D3;A.D4;A.D5;A.D6;A.D7;A.D8;A.D9;A.D10;A.D11;A.D12;A.D13;A.D14;A.D15;A.D16;A.D17;A.D18;A.D19;A.D20;A.D21;A.D22;A.D23;A.D24;A.D25;A.D26;A.D27;A.D28;A.D29;A.D30;A.D31] role))] | _ -> assert false) (List.init 32 Fun.id) in
 let codeLabels=StringOrder.Map.of_list [source,4;"target",4096;"str_data",24;"_float0",32] in let strings=StringOrder.Map.of_list [source,128;"str_data",4105] in let fs=StringOrder.Map.of_list [source,256;"_float0",8184] in let data=StringOrder.Map.of_list [source,512;S.coverageDataLabelName,8200;S.leakCounterLabelName,65536] in
 let labels=list (fun label -> list (fun offset -> list (fun instr -> tuple [J.armInstr instr;attempt word (fun () -> E.encodeWithLabels instr offset codeLabels strings fs data)]) [A.CBZ (A.X3,label);A.CBNZ (A.SP,label);A.B_label label;A.B_cond_label (A.HI,label);A.TBZ_label (A.X19,63,label);A.TBNZ_label (A.X28,32,label);A.BL label;A.ADRP (A.X1,label);A.ADR (A.X4,label);A.ADD_label (A.X16,A.SP,label);A.Label label]) [-4096;0;120;4095;4096;65536;Int32.to_int Int32.max_int;Int32.to_int Int32.min_int]) [source;"target";"str_data";"_float0";S.coverageDataLabelName;S.leakCounterLabelName;"missing"] in
 let literal=S.DataLabel (S.StringLiteral source) in let floating=S.DataLabel (S.FloatLiteral (-0.)) in
 let chunkGroups=[[];[[]];[[S.RET]];[[S.Label source;S.B_label source;S.ADR (A.X3,S.CodeLabel source);S.CBZ (A.X3,source)]];
 [[S.Label "start";S.B_label "local";S.MOVZ (A.X0,7,0);S.Label "local";S.BL "callee";S.ADRP (A.X1,literal);S.ADD_label (A.X1,A.X1,literal)];[S.Label "callee";S.RET]];
 [[S.Label source;S.MOVZ (A.X0,1,16)];[S.Label source;S.MOVK (A.X0,2,32);S.B_label source]];
 [[S.ADRP (A.X1,floating);S.ADD_label (A.X1,A.X1,floating);S.ADRP (A.X2,literal)];[S.ADRP (A.X3,literal);S.ADD_label (A.X3,A.X3,literal)]];
 [[S.BL "missing"]];[[S.ADRP (A.X1,S.DataLabel (S.Named S.leakCounterLabelName));S.ADD_label (A.X1,A.X1,S.DataLabel (S.Named S.leakCounterLabelName))]];
 [[S.FMOV_zero A.D0;S.FMOV_from_gp (A.D16,A.X6);S.CNT_8B (A.D16,A.D16);S.ADDV_8B (A.D16,A.D16);S.UMOV_byte (A.X5,A.D16)]];
 [[S.ADRP (A.X1,S.DataLabel (S.FloatLiteral (Int64.float_of_bits 0x7ff8000000000001L)));S.ADRP (A.X2,S.DataLabel (S.FloatLiteral 0.));S.ADRP (A.X3,floating)]]] in
 let chunks=list (fun group -> let flattened=List.concat group in let collected=ARM64_Resolve.collectPools flattened in
 tuple [list (list J.symInstr) group;map i (E.computeSymbolicLabelPositions flattened);tuple [i (fst (E.computeSymbolicLayout flattened));map i (snd (E.computeSymbolicLayout flattened))];i (E.getSymbolicCodeSize flattened);pools collected;
 attempt (fun preparedChunks -> let refs=List.to_seq preparedChunks |> Seq.flat_map (fun (p:E.preparedChunk) -> Array.to_seq p.E.poolLabelRefs) in
 let collectedRefs=ARM64_Resolve.collectPoolsFromLabelRefs refs in
 tuple [list prepared preparedChunks;pools collectedRefs;attempt prepared (fun () -> E.combinePreparedChunks preparedChunks);
 list (fun (sp,fp) -> tuple [stringPool sp;floatPool fp;i (E.getStringPoolSize sp);i (E.getFloatPoolSize fp);list (fun os -> list (fun leak -> tuple [attempt (array word) (fun () -> E.encodeSymbolicWithPools flattened sp fp os leak);attempt (array word) (fun () -> E.encodePreparedChunksWithPools preparedChunks sp fp os leak);attempt (array word) (fun () -> E.encodePreparedChunksWithPools [E.combinePreparedChunks preparedChunks] sp fp os leak)]) [false;true]) [Platform.Linux;Platform.MacOS]]) [collected;collectedRefs;LiteralPool.emptyStringPool,LiteralPool.emptyFloatPool;LiteralPool.createStringPool (List.to_seq ["";source;"é";source;"123456789"]),LiteralPool.createFloatPool (List.to_seq [0.;-0.;1.;-0.])]]) (fun () -> List.map E.prepareSymbolicChunk group)]) chunkGroups in
 let concreteStreams=[[];[A.Label source;A.RET];[A.Label source;A.B_label source];[A.Label source;A.Label source;A.MOVZ (A.X0,4,0)];[A.BL "missing"];[A.FMOV_imm (A.D0,0.1)]] in
 let concrete=list (fun instrs -> tuple [map i (E.computeLabelPositions instrs);i (E.getCodeSize instrs);list (fun os -> list (fun leak -> attempt (array word) (fun () -> E.encodeAllWithPools instrs LiteralPool.emptyStringPool LiteralPool.emptyFloatPool os leak)) [false;true]) [Platform.Linux;Platform.MacOS]]) concreteStreams in
 let leakLabels=list (fun os -> list (fun offset -> list (fun codeSize -> list (fun (fpSize,spSize) -> map i (E.computeLeakCounterLabel os offset codeSize fpSize spSize)) [0,0;8,16;24,64;65536,65536]) [0;4;7;65535;Int32.to_int Int32.max_int]) [0;120;792;65536;Int32.to_int Int32.max_int]) [Platform.Linux;Platform.MacOS] in
 tuple [instructions;logical;floats;helperRegs;labels;chunks;concrete;leakLabels]
