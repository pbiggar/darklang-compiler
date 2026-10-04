(* Exact byte observations for all x64 encoding forms and operand boundaries. *)
[@@@warning "-4"]
open Dark_compiler
open X64EncodingFixtures
let tuple values=`Assoc ["tuple",`List values]
let list f xs=`List (List.map f xs)
let bytes value=`List (List.init (Bytes.length value) (fun i -> `Assoc ["kind",`String "uint8";"value",`String (string_of_int (Char.code (Bytes.get value i)))]))
let attempt instr=try SemanticJson.union "FSharpResult" "Ok" [bytes (X86_64_Encoding.encodeInstruction instr)] with Failure msg | Invalid_argument msg -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string msg]
let observation instr=tuple [MachineISAObservation.x64Instr instr;attempt instr]
let observe source=
 let boundaries=[-2147483648;-129;-128;-127;-1;0;1;2;4;8;127;128;255;256;2147483647] in
 let instructions=list (fun left -> list (fun right -> list (fun boundary -> list observation (instructions source left right boundary)) boundaries) (List.init 16 Fun.id)) (List.init 16 Fun.id) in
 let indexes=list (fun dest -> list (fun base -> list (fun index -> list (fun scale -> list (fun offset -> observation (X86_64.LEA_index (dest,base,index,scale,Int32.of_int offset))) [-129;-128;-1;0;1;127;128;Int32.to_int Int32.min_int;Int32.to_int Int32.max_int]) [-1;0;1;2;3;4;8;16]) (Array.to_list regValues)) (Array.to_list regValues)) (Array.to_list regValues) in
 let wide=list (fun reg -> list (fun value -> observation (X86_64.MOV_imm (reg,value))) [Int64.min_int;-2147483649L;-2147483648L;-1L;0L;2147483647L;2147483648L;Int64.max_int]) (Array.to_list regValues) in
 let conditions=list (fun cond -> list (fun dest -> list (fun src -> list observation [X86_64.SETcc (cond,dest);X86_64.CMOVcc (cond,dest,src);X86_64.Jcc (cond,source)]) (Array.to_list regValues)) (Array.to_list regValues)) (Array.to_list conditionValues) in
 let fpMixed=list (fun dest -> list (fun src -> list observation [X86_64.CVTSI2SD (dest,src);X86_64.CVTTSD2SI (src,dest);X86_64.MOVQ_to_gp (src,dest);X86_64.MOVQ_from_gp (dest,src)]) (Array.to_list regValues)) (Array.to_list fRegValues) in
 tuple [instructions;indexes;wide;conditions;fpMixed]
