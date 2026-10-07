(* X64EmitPrinting.ml - Emit x64 instructions for printing operations. *)
[@@@warning "-4"]

open X64Operands
open X64Printing
open X64CodeGenTypes
module X = X86_64

let utf8Bytes = Bytes.of_string
let emitPrintChars (_ctx : funcCtx) bytes = Ok (genPrintChars bytes)

let emitPrintInt64 (_ctx : funcCtx) reg =
  resolveReg reg |> Result.map (fun srcReg -> genPrintInt64 srcReg true)

let emitPrintUInt64 (_ctx : funcCtx) reg =
  resolveReg reg |> Result.map (fun srcReg -> genPrintUInt64 srcReg true)

let emitPrintInt64NoNewline (_ctx : funcCtx) reg =
  resolveReg reg |> Result.map (fun srcReg -> genPrintInt64 srcReg false)

let emitPrintUInt64NoNewline (_ctx : funcCtx) reg =
  resolveReg reg |> Result.map (fun srcReg -> genPrintUInt64 srcReg false)

(*
   Print "true\n" or "false\n" without exiting (Ret handles exit)
   "false\n"
   "true\n"
*)
let printBool srcReg newline =
  let trueLabel = freshLabel "bool_true" in
  let writeLabel = freshLabel "bool_write" in
  let ending = if newline then 1 else 0 in
  [
    X.TEST_reg (srcReg, srcReg); X.Jcc (X.NE, trueLabel); X.SUB_imm (X.RSP, 8l);
  ]
  @ loadImm64 scratch (if newline then 0x0a65736c6166L else 0x65736c6166L)
  @ [
      X.MOV_store (X.RSP, 0l, scratch);
      X.MOV_imm32 (X.RDX, Int32.of_int (5 + ending));
      X.JMP writeLabel;
      X.Label trueLabel;
      X.SUB_imm (X.RSP, 8l);
    ]
  @ loadImm64 scratch (if newline then 0x0a65757274L else 0x65757274L)
  @ [
      X.MOV_store (X.RSP, 0l, scratch);
      X.MOV_imm32 (X.RDX, Int32.of_int (4 + ending));
      X.Label writeLabel;
      X.MOV_reg (X.RSI, X.RSP);
      X.MOV_imm32 (X.RDI, 1l);
    ]
  @ genWriteSyscall
  @ [ X.ADD_imm (X.RSP, 8l) ]

let emitPrintBool (_ctx : funcCtx) reg =
  resolveReg reg |> Result.map (fun srcReg -> printBool srcReg true)

let emitPrintBoolNoNewline (_ctx : funcCtx) reg =
  resolveReg reg |> Result.map (fun srcReg -> printBool srcReg false)

(*
   Dynamic string format: [refcount:8][length:8][data:N]
   Print data + newline (exit handled by subsequent Ret → epilogue)
*)
let emitPrintHeapString (_ctx : funcCtx) reg =
  resolveReg reg
  |> Result.map (fun srcReg ->
      [ X.PUSH srcReg ]
      @ (if srcReg = X.RDX then
           [
             X.MOV_reg (X.R10, srcReg);
             X.MOV_load (X.RDX, X.R10, 8l);
             X.LEA (X.RSI, X.R10, 16l);
           ]
         else [ X.MOV_load (X.RDX, srcReg, 8l); X.LEA (X.RSI, srcReg, 16l) ])
      @ [ X.MOV_imm32 (X.RDI, 1l) ]
      @ genWriteSyscall
      @ [ X.SUB_imm (X.RSP, 8l) ]
      @ loadImm64 scratch 10L
      @ [
          X.MOV_store (X.RSP, 0l, scratch);
          X.MOV_imm32 (X.RDI, 1l);
          X.MOV_reg (X.RSI, X.RSP);
          X.MOV_imm32 (X.RDX, 1l);
        ]
      @ genWriteSyscall
      @ [ X.ADD_imm (X.RSP, 8l); X.POP srcReg ])

let emitPrintHeapStringNoNewline (_ctx : funcCtx) reg =
  resolveReg reg
  |> Result.map (fun srcReg ->
      [ X.PUSH srcReg ]
      @ (if srcReg = X.RDX then
           [
             X.MOV_reg (X.R10, srcReg);
             X.MOV_load (X.RDX, X.R10, 8l);
             X.LEA (X.RSI, X.R10, 16l);
           ]
         else [ X.MOV_load (X.RDX, srcReg, 8l); X.LEA (X.RSI, srcReg, 16l) ])
      @ [ X.MOV_imm32 (X.RDI, 1l) ]
      @ genWriteSyscall @ [ X.POP srcReg ])

(*
   Write a literal string to stdout and exit(0)
   fd = stdout
*)
let emitPrintString (_ctx : funcCtx) str =
  let bytes = utf8Bytes (str ^ "\n") in
  let len = Bytes.length bytes in
  let padded = (len + 7) / 8 * 8 in
  let paddedBytes = Bytes.make padded '\000' in
  Bytes.blit bytes 0 paddedBytes 0 len;
  let pushInstrs =
    List.init (padded / 8) (fun index ->
        Bytes.get_int64_le paddedBytes (index * 8))
    |> List.rev
    |> List.concat_map (fun value ->
        loadImm64 scratch value @ [ X.PUSH scratch ])
  in
  Ok
    (pushInstrs
    @ [ X.MOV_imm32 (X.RDI, 1l); X.MOV_reg (X.RSI, X.RSP) ]
    @ loadImm64 X.RDX (Int64.of_int len)
    @ genWriteSyscall
    @ [ X.ADD_imm (X.RSP, Int32.of_int padded) ]
    @ loadImm64 X.RDI 0L @ genExitSyscall)

(* Float.toString transfers ownership of its temporary string to the printer. *)
let printFloat ctx freg newline =
  match freg with
  | LIR.FPhysical fp ->
      let xmm = lirFRegToX86 fp in
      Result.map
        (fun release ->
          [ X.PUSH X.R12; X.PUSH X.R13 ]
          @ (if xmm <> X.XMM0 then [ X.MOVSD_reg (X.XMM0, xmm) ] else [])
          @ [
              X.CALL "Darklang.Stdlib.Float.toString";
              X.MOV_reg (X.R12, X.RAX);
              X.MOV_load (X.RDX, X.R12, 8l);
              X.LEA (X.RSI, X.R12, 16l);
              X.MOV_imm32 (X.RDI, 1l);
            ]
          @ genWriteSyscall @ release
          @ [ X.POP X.R13; X.POP X.R12 ]
          @ if newline then genPrintChars [ '\n' ] else [])
        (X64EmitReferenceCounts.emitRefCountDecString ctx
           (LIR.Reg (LIR.Physical LIR.X20)))
  | _ -> Error "PrintFloat with virtual FP register"

let emitPrintFloat ctx freg = printFloat ctx freg true
let emitPrintFloatNoNewline ctx freg = printFloat ctx freg false

(* Aggregate printers preserve their traversal registers across scalar printers
   and nested aggregates. They return normally so caller cleanup still runs. *)
let ( let* ) = Result.bind
let literal text = genPrintChars (List.of_seq (String.to_seq text))

let withPointer reg body =
  [ X.PUSH X.R12; X.PUSH X.R13 ]
  @ (if reg = X.RSP then [ X.LEA (X.R12, X.RSP, 16l) ]
     else [ X.MOV_reg (X.R12, reg) ])
  @ body
  @ [ X.POP X.R13; X.POP X.R12 ]

let rec printValue ctx typ reg =
  match typ with
  | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16
  | AST.TUInt32 ->
      Ok (genPrintInt64 reg false)
  | AST.TUInt64 -> Ok (genPrintUInt64 reg false)
  | AST.TBool -> Ok (printBool reg false)
  | AST.TString | AST.TChar | AST.TInt | AST.TInt128 | AST.TUInt128 ->
      (* Copy the buffer address before setting the write syscall arguments. *)
      Ok
        ([
           X.MOV_reg (X.R10, reg);
           X.MOV_load (X.RDX, X.R10, 8l);
           X.LEA (X.RSI, X.R10, 16l);
           X.MOV_imm32 (X.RDI, 1l);
         ]
        @ genWriteSyscall)
  | AST.TFloat64 ->
      let* printed = emitPrintFloatNoNewline ctx (LIR.FPhysical LIR.D0) in
      Ok ([ X.MOVQ_from_gp (X.XMM0, reg) ] @ printed)
  | AST.TUnit -> Ok (literal "()")
  | AST.TBlob -> Ok (literal "<Blob: ephemeral>")
  | AST.TList elem -> printList ctx reg elem
  | AST.TTuple elems ->
      let* fields = printFields ctx elems in
      Ok (withPointer reg (literal "(" @ fields @ literal ")"))
  | AST.TRecord (name, []) -> (
      match StringOrder.Map.find_opt name ctx.recordRegistry with
      | Some fields -> printRecord ctx reg name fields
      | None ->
          Error ("Print: Record type '" ^ name ^ "' not found in recordRegistry")
      )
  | typ ->
      Error
        ("Unsupported nested x64 print type: "
        ^ CheckingDiagnostics.typeToString typ)

and printFields ctx types =
  ResultList.traverse
    (fun (index, typ) ->
      let* printed = printValue ctx typ X.RAX in
      Ok
        ((if index = 0 then [] else literal ", ")
        @ [ X.MOV_load (X.RAX, X.R12, Int32.of_int (index * 8)) ]
        @ printed))
    (List.mapi (fun index typ -> (index, typ)) types)
  |> Result.map List.concat

and printList ctx reg elemType =
  let* printed = printValue ctx elemType X.RAX in
  let loop = freshLabel "print_list_loop"
  and ending = freshLabel "print_list_end"
  and head = freshLabel "print_list_head" in
  Ok
    (withPointer reg
       (literal "["
       @ [
           X.MOV_imm32 (X.R13, 1l);
           X.Label loop;
           X.TEST_reg (X.R12, X.R12);
           X.Jcc (X.EQ, ending);
           X.TEST_reg (X.R13, X.R13);
           X.Jcc (X.NE, head);
         ]
       @ literal ", "
       @ [
           X.Label head; X.MOV_imm32 (X.R13, 0l); X.MOV_load (X.RAX, X.R12, 8l);
         ]
       @ printed
       @ [ X.MOV_load (X.R12, X.R12, 16l); X.JMP loop; X.Label ending ]
       @ literal "]"))

and printRecord ctx reg name fields =
  let* printed =
    ResultList.traverse
      (fun (index, (name, typ)) ->
        let* value = printValue ctx typ X.RAX in
        Ok
          ((if index = 0 then [] else literal ", ")
          @ literal (name ^ " = ")
          @ [ X.MOV_load (X.RAX, X.R12, Int32.of_int (index * 8)) ]
          @ value))
      (List.mapi (fun index field -> (index, field)) fields)
  in
  Ok
    (withPointer reg
       (literal (name ^ " { ") @ List.concat printed @ literal " }"))

let emitPrintList ctx reg elemType =
  let* reg = resolveReg reg in
  let* printed = printList ctx reg elemType in
  Ok (printed @ literal "\n")

let emitPrintRecord ctx reg name fields =
  let* reg = resolveReg reg in
  let* printed = printRecord ctx reg name fields in
  Ok (printed @ literal "\n")

let emitPrintBlob (_ctx : funcCtx) reg =
  let* _ = resolveReg reg in
  Ok (literal "<Blob: ephemeral>\n")

let emitPrintSum ctx reg variants transparentPayload =
  let* reg = resolveReg reg in
  if variants = [] then Error "Cannot print a sum with no variants"
  else
    let nullableTags =
      if transparentPayload || List.length variants <> 2 then None
      else
        match
          ( List.find_map
              (fun (_, tag, payload) ->
                if payload = None then Some tag else None)
              variants,
            List.find_map
              (fun (_, tag, payload) ->
                if payload = Some AST.TString then Some tag else None)
              variants )
        with
        | Some absent, Some present -> Some (absent, present)
        | _ -> None
    in
    let ending = freshLabel "print_sum_end" in
    let* blocks =
      ResultList.traverse
        (fun (name, tag, payload) ->
          let next = freshLabel "print_sum_next" in
          let* payload =
            match payload with
            | None -> Ok []
            | Some typ ->
                let* value = printValue ctx typ X.RAX in
                let load =
                  if transparentPayload || Option.is_some nullableTags then
                    X.MOV_reg (X.RAX, X.R12)
                  else X.MOV_load (X.RAX, X.R12, 8l)
                in
                Ok (literal "(" @ [ load ] @ value @ literal ")")
          in
          Ok
            ((if transparentPayload then []
              else [ X.CMP_imm (X.R13, Int32.of_int tag); X.Jcc (X.NE, next) ])
            @ literal name @ payload
            @ [ X.JMP ending; X.Label next ]))
        variants
    in
    let setup =
      if transparentPayload then []
      else
        match nullableTags with
        | Some (absent, present) ->
            let nonnull = freshLabel "print_sum_present"
            and ready = freshLabel "print_sum_ready" in
            [
              X.TEST_reg (X.R12, X.R12);
              X.Jcc (X.NE, nonnull);
              X.MOV_imm32 (X.R13, Int32.of_int absent);
              X.JMP ready;
              X.Label nonnull;
              X.MOV_imm32 (X.R13, Int32.of_int present);
              X.Label ready;
            ]
        | None ->
            if
              List.exists
                (fun (_, _, payload) -> Option.is_some payload)
                variants
            then [ X.MOV_load (X.R13, X.R12, 0l) ]
            else [ X.MOV_reg (X.R13, X.R12) ]
    in
    Ok
      (withPointer reg
         (setup @ List.concat blocks @ [ X.Label ending ] @ literal "\n"))
