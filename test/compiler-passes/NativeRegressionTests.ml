(* NativeRegressionTests.ml - Observable Unicode, Mach-O and x64 printing repairs. *)
[@@@warning "-4-42"]
open Dark_compiler
module X=X86_64
let ( let* )=Result.bind
let require condition message=if condition then Ok () else Error message
let context : X64CodeGenTypes.funcCtx =
 {X64CodeGenTypes.functionName="native-printing";stackSize=0;usedCalleeSaved=[];enableLeakCheck=false;
  recordRegistry=StringOrder.Map.empty;sumShapeRegistry=StringOrder.Map.empty;functionNames=FunctionIdMap.empty}
let textTests=[
 "native Unicode ordering and case folding",(fun () ->
  require (StringOrder.Set.elements (StringOrder.Set.of_list ["𐀀";"a";"\238\128\128"])
    =["a";"\238\128\128";"𐀀"] && Text.caseFold "StraßeΣς"="strasseσσ")
    "Compiler text did not use native ordering or Unicode case folding");
 "native Unicode grapheme consistency",(fun () ->
  require (Text.graphemeClusters "क्ष😀👩‍💻"=["क्ष";"😀";"👩‍💻"])
    "Indic conjunct or emoji segmentation differs from extended graphemes");
 "native Unicode first grapheme",(fun () ->
  require (List.for_all (fun text -> Text.firstGrapheme text =
    List.nth_opt (Text.graphemeClusters text) 0)
    ["";"a";"érest";"क्षrest";"😀rest";"👩‍💻rest";"🇮🇪rest";"\r\nrest"])
    "First grapheme differs from standard extended grapheme segmentation");
 "native Unicode scalars and normalization",(fun () ->
  require (Text.scalars "𐐨😀"=[|0x10428;0x1f600|] && Text.normalize "é"="é"
    && Text.normalize (Text.ofScalars [|0xfffe|])=Text.ofScalars [|0xfffe|]
    && Text.lowerInvariant "𐐀"="𐐨") "Native Unicode text differs");
 "native Unicode malformed text rejection",(fun () ->
  let rejects text=try ignore (Text.scalars text);false with Invalid_argument _ -> true in
  require (List.for_all rejects ["\237\160\128";"\192\128";"\244\144\128\128"])
    "Invalid UTF-8 was accepted as source text");
 "native Unicode ordinal helper prefixes",(fun () ->
  require (Text.startsWith "Darklang.Stdlib.Dict.get" "Darklang.Stdlib."
    && not (Text.startsWith "Darklang.Std\226\128\141lib.Dict.get" "Darklang.Stdlib.")
    && not (Text.startsWith "Darklang.Std\000lib.Dict.get" "Darklang.Stdlib.")
    && not (Text.endsWith "x\226\128\141json" "xjson")
    && not (ARM64CodeGenTypes.callerOwnsSinglePayloadSum "Darklang.Stdlib.String.test")
    && ARM64CodeGenTypes.callerOwnsSinglePayloadSum "Darklang.Std\226\128\141lib.String.test")
    "Compiler helper classification used linguistic collation");
 "native Unicode dictionary diagnostic key types",(fun () ->
  require (CheckingDiagnostics.typeErrorToString (CheckingDiagnostics.TypeMismatch
    (AST.TDict (AST.TString,AST.TInt64),AST.TDict (AST.TInt64,AST.TInt64),"keys"))
    ="Type mismatch in keys: expected Dict<String, Int64>, got Dict<Int64, Int64>")
    "Dictionary mismatch hid the key types")]
let machoImage words strings floats =
 let image=Binary_Generation_MachO.createExecutableWithPools words
   (LiteralPool.createStringPool (List.to_seq strings)) (LiteralPool.createFloatPool (List.to_seq floats)) false in
 let size=Bytes.length image in
 let rec commands offset remaining found = if remaining=0 then Ok found else
  let command=Bytes.get_int32_le image offset in let length=Int32.to_int (Bytes.get_int32_le image (offset+4)) in
  if length<8 || offset+length>size then Error "Invalid Mach-O load command extent" else
  commands (offset+length) (remaining-1) ((command,offset)::found) in
 let* commands=commands 32 (Int32.to_int (Bytes.get_int32_le image 16)) [] in
 let segment name=List.find_map (fun (command,offset) ->
   if command=Binary.lc_SEGMENT_64 && Bytes.sub_string image (offset+8) (String.length name)=name then Some offset else None) commands in
 match segment "__TEXT",segment "__LINKEDIT" with
 | Some text,Some linkedit ->
  let fileSize=Bytes.get_int64_le image (text+48) and vmSize=Bytes.get_int64_le image (text+32) in
  let codeOffset=Int32.to_int (Bytes.get_int32_le image (text+72+48)) in
  let* ()=require (fileSize=Int64.of_int size && vmSize=fileSize && size mod 16384=0
    && Bytes.get_int64_le image (linkedit+40)=fileSize
    && Bytes.get_int64_le image (linkedit+24)=Int64.add (Bytes.get_int64_le image (text+24)) vmSize)
    "Mach-O segment extent or LINKEDIT position differs" in
  let* ()=require (Array.for_all (fun index -> Bytes.get_int32_le image (codeOffset+index*4)=words.(index))
    (Array.init (Array.length words) Fun.id)) "Mach-O code was truncated or moved" in
  let sections=Int32.to_int (Bytes.get_int32_le image (text+64)) in
  let fits=List.for_all (fun index -> let section=text+72+index*80 in
    let offset=Int64.of_int32 (Bytes.get_int32_le image (section+48)) in
    let length=Bytes.get_int64_le image (section+40) in
    Int64.add offset length<=fileSize) (List.init sections Fun.id) in
  require fits "Mach-O constant section extends outside __TEXT"
 | _ -> Error "Missing Mach-O text or linkedit segment"
let machoTests=List.map (fun words -> "Mach-O grows beyond fixed text segment: "^string_of_int words,
  fun () -> machoImage (Array.make words 0xd503201fl) [] []) [0;1;3800;3900;4096;12000]
 @ ["Mach-O includes large UTF-8 constants and floats",(fun () ->
   machoImage [|0xd65f03c0l|] [String.concat "" (List.init 10000 (fun _ -> "😀hé"))] [0.;-0.;42.])]
let literal text=X64Operands.genPrintChars (List.of_seq (String.to_seq text))
let checkOutput expected body =
 let* resolved=X86_64_Resolve.resolveAndEncode
   ([X.Label "_start"] @ body @ literal "|continued" @ X64Operands.loadImm64 X.RDI 0L @ X64Operands.genExitSyscall) in
 let image=Binary_Generation_ELF_X86_64.createExecutableWithPools resolved.X86_64_Resolve.machineCode
   LiteralPool.emptyStringPool LiteralPool.emptyFloatPool false 0 in
 let path=Filename.temp_file "native-printing-" ".elf" in
 Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
  Out_channel.with_open_bin path (fun channel -> Out_channel.output_bytes channel image);
  Unix.chmod path 0o700;
  match ProcessCapture.capture path [] 10000 with
  | Ok (0,output,"") -> require (output=expected^"|continued") (Printf.sprintf "Expected %S, got %S" (expected^"|continued") output)
  | Ok (code,output,errors) -> Error (Printf.sprintf "Printer exited %d: %S %S" code output errors)
  | Error message -> Error message)
let withStack bytes body=[X.SUB_imm (X.RSP,Int32.of_int bytes)] @ body @ [X.ADD_imm (X.RSP,Int32.of_int bytes)]
let word offset value=X64Operands.loadImm64 X.RAX value @ [X.MOV_store (X.RSP,Int32.of_int offset,X.RAX)]
let pointer reg offset=[X.LEA (reg,X.RSP,Int32.of_int offset)]
let stringBuffer text =
 let data=Bytes.make (((String.length text+7)/8)*8) '\000' in Bytes.blit_string text 0 data 0 (String.length text);
 word 0 Int64.max_int @ word 8 (Int64.of_int (String.length text))
 @ List.concat (List.init (Bytes.length data/8) (fun index -> word (16+index*8) (Bytes.get_int64_le data (index*8))))
let printingTests =
 List.map (fun value -> "x64 boolean printer returns without newline: "^Int64.to_string value,
  fun () -> let* code=X64EmitPrinting.emitPrintBoolNoNewline context (LIR.Physical LIR.X0) in
   checkOutput (if value=0L then "false" else "true") (X64Operands.loadImm64 X.RAX value @ code)) [0L;1L;-1L]
 @ List.map (fun (reg,actual) -> "x64 list printer returns and supports "^(match reg with LIR.X0->"X0"|LIR.X1->"X1"|LIR.X7->"X7"|LIR.X20->"X20"|LIR.X21->"X21"|_->"other"),
  fun () -> let* code=X64EmitPrinting.emitPrintList context (LIR.Physical reg) AST.TInt64 in
   let nodes=word 0 1L @ word 8 42L @ pointer X.RAX 24 @ [X.MOV_store (X.RSP,16l,X.RAX)]
    @ word 24 1L @ word 32 (-7L) @ word 40 0L @ pointer actual 0 in
   checkOutput "[42, -7]\n" (withStack 48 (nodes @ code)))
   [LIR.X0,X.RAX;LIR.X1,X.RDI;LIR.X7,X.RDX;LIR.X20,X.R12;LIR.X21,X.R13]
 @ ["x64 float printer releases its temporary string",(fun () ->
   let* code=X64EmitPrinting.emitPrintFloatNoNewline context (LIR.FPhysical LIR.D0) in
   let doneLabel="float_print_done" in
   checkOutput "1.5|refs=0" (withStack 32
     (stringBuffer "1.5" @ word 0 1L @ code @ literal "|refs="
      @ [X.MOV_load (X.RAX,X.RSP,0l)] @ X64Printing.genPrintInt64 X.RAX false
      @ [X.JMP doneLabel;X.Label "Darklang.Stdlib.Float.toString";
         X.LEA (X.RAX,X.RSP,24l);X.RET;X.Label doneLabel])));
  "x64 empty list printer returns",(fun () ->
   let* code=X64EmitPrinting.emitPrintList context (LIR.Physical LIR.X0) AST.TBool in
   checkOutput "[]\n" (X64Operands.loadImm64 X.RAX 0L @ code));
  "x64 tuple list printer retains traversal state",(fun () ->
   let* code=X64EmitPrinting.emitPrintList context (LIR.Physical LIR.X0) (AST.TTuple [AST.TInt64;AST.TBool]) in
   checkOutput "[(42, true)]\n" (withStack 48 (word 0 1L @ pointer X.RAX 24 @ [X.MOV_store (X.RSP,8l,X.RAX)]
    @ word 16 0L @ word 24 42L @ word 32 1L @ pointer X.RAX 0 @ code)));
  "x64 record printer returns",(fun () ->
   let* code=X64EmitPrinting.emitPrintRecord context (LIR.Physical LIR.X0) "Point" ["x",AST.TInt64;"visible",AST.TBool] in
   checkOutput "Point { x = 42, visible = true }\n" (withStack 16 (word 0 42L @ word 8 1L @ pointer X.RAX 0 @ code)));
  "x64 blob printer remains opaque and returns",(fun () ->
   let* code=X64EmitPrinting.emitPrintBlob context (LIR.Physical LIR.X0) in checkOutput "<Blob: ephemeral>\n" code);
  "x64 boxed sum printer returns",(fun () ->
   let* code=X64EmitPrinting.emitPrintSum context (LIR.Physical LIR.X0) ["Number",1,Some AST.TInt64;"Absent",0,None] false in
   checkOutput "Number(42)\n" (withStack 16 (word 0 1L @ word 8 42L @ pointer X.RAX 0 @ code)));
  "x64 transparent sum printer returns",(fun () ->
   let* code=X64EmitPrinting.emitPrintSum context (LIR.Physical LIR.X0) ["Visible",0,Some AST.TBool] true in
   checkOutput "Visible(true)\n" (X64Operands.loadImm64 X.RAX 1L @ code));
  "x64 nullary sum printer returns",(fun () ->
   let* code=X64EmitPrinting.emitPrintSum context (LIR.Physical LIR.X0) ["A",0,None;"B",7,None] false in
   checkOutput "B\n" (X64Operands.loadImm64 X.RAX 7L @ code));
  "x64 nullable string sum printer handles absence",(fun () ->
   let* code=X64EmitPrinting.emitPrintSum context (LIR.Physical LIR.X0) ["None",0,None;"Some",1,Some AST.TString] false in
   checkOutput "None\n" (X64Operands.loadImm64 X.RAX 0L @ code));
  "x64 nullable string sum printer preserves UTF-8",(fun () ->
   let* code=X64EmitPrinting.emitPrintSum context (LIR.Physical LIR.X0) ["None",0,None;"Some",1,Some AST.TString] false in
   checkOutput "Some(hé😀)\n" (withStack 32 (stringBuffer "hé😀" @ pointer X.RAX 0 @ code)))]
