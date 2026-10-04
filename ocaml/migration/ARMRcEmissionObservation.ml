(* Complete RC instruction results, scratch aliases, buffer guards and caller ownership. *)
open Dark_compiler
open! MemoryModel
module E=ARM64EmitReferenceCounts
let tuple xs=`Assoc ["tuple",`List xs]
let list f xs=`List (List.map f xs)
let call f=try tuple [`Bool false;(match f () with Ok xs -> SemanticJson.union "FSharpResult" "Ok" [list MachineISAObservation.symInstr xs] | Error error -> SemanticJson.union "FSharpResult" "Error" [SemanticJson.string error])] with Failure _ | Invalid_argument _ -> tuple [`Bool true]
let observe source=
 let physical=[LIR.X0;LIR.X1;LIR.X2;LIR.X3;LIR.X4;LIR.X5;LIR.X6;LIR.X7;LIR.X8;LIR.X9;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X16;LIR.X17;LIR.X19;LIR.X20;LIR.X21;LIR.X22;LIR.X23;LIR.X24;LIR.X25;LIR.X26;LIR.X27;LIR.X29;LIR.X30;LIR.SP] in
 let regs=List.map (fun reg -> LIR.Physical reg) physical@[LIR.Virtual (-1);LIR.Virtual 0;LIR.Virtual 2147483647] in
 let sizes=[-2147483648;-65536;-32769;-32768;-1;0;8;16;248;255;256;32767;32768;65535;65536;2147483647] in
 let kinds=[LIR.GenericHeap;LIR.StreamHeap;LIR.TaggedList;LIR.DictHeap;LIR.ClosureHeap] in
 let dynamic=DynamicBufferRelease DynamicStringBuffer in
 let simple=[NoReleasePlan;dynamic;DynamicBufferRelease DynamicBlobBuffer;DynamicBufferRelease DynamicIntBuffer;DynamicBufferRelease (FixedSizeRoot (8,GenericHeap));RecursiveRelease (AST.TRecord (source,[]))]@List.concat_map (fun kind -> [RootRelease (8,kind,NoPayloadRelease);RootRelease (8,kind,TaggedListPayloadRelease dynamic);RootRelease (8,kind,DictPayloadRelease (NoReleasePlan,dynamic));RootRelease (8,kind,ClosurePayloadRelease [])]) [GenericHeap;StreamHeap;TaggedList;DictHeap;ClosureHeap] in
 let child=RootRelease (8,GenericHeap,FixedBlockPayloadRelease (8,[FieldRelease (0,dynamic)])) in
 let fields=List.mapi (fun index plan -> FieldRelease (index*8,plan)) (simple@[child]) in
 let variants=[{tag=0;fieldReleases=[]};{tag=1;fieldReleases=fields};{tag=65536;fieldReleases=[FieldRelease (-32769,child)]}] in
 let rich=[RootRelease (216,GenericHeap,FixedBlockPayloadRelease (216,fields));RootRelease (216,GenericHeap,BoxedSumPayloadRelease (216,fields,variants));RootRelease (16,GenericHeap,BoxedSumPayloadRelease (16,[FieldRelease (8,dynamic)],[{tag=1;fieldReleases=[FieldRelease (8,dynamic)]}]));RootRelease (8,GenericHeap,BoxedSumPayloadRelease (8,[],[]))] in
 let meta plan=Some {releasePlanCacheKey=Some source;releasePlan=Some plan;sourceType=Some AST.TString} in
 let metadata=None::Some {releasePlanCacheKey=None;releasePlan=None;sourceType=None}::List.map meta (simple@rich) in
 let operands=List.map (fun reg -> LIR.Reg reg) regs@[LIR.Imm 0L;LIR.Imm 1L;LIR.Imm Int64.min_int;LIR.Imm Int64.max_int;LIR.StackSlot (-1);LIR.StackSlot 0;LIR.StringSymbol source;LIR.StringSymbol ""] in
 list (fun target -> list (fun enabled ->
  let ctx=ARMPrintingObservation.context source target enabled in
  let basic=list (fun reg -> list (fun size -> list (fun kind -> tuple [call (fun () -> E.emitRefCountInc ctx reg size kind);call (fun () -> E.emitRefCountDec ctx reg size kind (meta NoReleasePlan))]) kinds) sizes) regs in
  let structural=list (fun reg -> list (fun metadata -> list (fun kind -> call (fun () -> E.emitRefCountDec ctx reg 16 kind metadata)) kinds) metadata) (List.map (fun reg -> LIR.Physical reg) [LIR.X0;LIR.X10;LIR.X11;LIR.X12;LIR.X13;LIR.X14;LIR.X15;LIR.X19]) in
  let callers=list (fun name -> let ctx={ctx with ARM64CodeGenTypes.functionName=name} in list (fun plan -> call (fun () -> E.emitRefCountDec ctx (LIR.Physical LIR.X12) 16 LIR.GenericHeap (meta plan))) rich) [source;"Darklang.Stdlib.List.foo";"Darklang.Stdlib.Dict.foo"] in
  let buffers=list (fun operand -> tuple [call (fun () -> E.emitRefCountIncString ctx operand);call (fun () -> E.emitRefCountDecString ctx operand);call (fun () -> E.emitRefCountIncInt ctx operand);call (fun () -> E.emitRefCountDecInt ctx operand)]) operands in
  tuple [basic;structural;callers;buffers]) [false;true]) [ARM64.targetConfigFor Platform.LinuxARM64;ARM64.targetConfigFor Platform.MacOSARM64]
