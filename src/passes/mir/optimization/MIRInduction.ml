(* MIRInduction.ml - Reduce affine induction expressions in verified loop shapes. *)
[@@@warning "-4"]

open MIR
module F = MIROptimizationFacts
module T = MIRLoopTopology

type affineInductionCandidate = {
  header : label;
  preheader : label;
  latch : label;
  initialValue : operand;
  affineValue : vReg;
  coefficient : operand;
  preheaderCoefficient : operand;
  _offset : operand;
  preheaderOffset : operand;
  offsetOperator : binOp;
  valueType : AST.semanticType;
  scaleInstr : instr;
  affineInstr : instr;
}

let addInt value delta =
  Int32.to_int (Int32.add (Int32.of_int value) (Int32.of_int delta))

let nextRegisterId (cfg : cfg) =
  let registers =
    LabelMap.fold
      (fun _ block registers ->
        let registers =
          List.fold_left
            (fun registers instr ->
              let registers = VRegSet.union registers (F.getInstrUses instr) in
              match F.getInstrDest instr with
              | Some dest -> VRegSet.add dest registers
              | None -> registers)
            registers block.instrs
        in
        VRegSet.union registers (F.getTerminatorUses block.terminator))
      cfg.blocks VRegSet.empty
  in
  addInt
    (VRegSet.fold (fun (VReg id) highest -> max highest id) registers (-1))
    1

let resolveLatchCopy instrs register =
  let rec resolve seen current =
    if VRegSet.mem current seen then current
    else
      match
        List.find_map
          (function
            | Mov (dest, Register source, _) when dest = current -> Some source
            | _ -> None)
          instrs
      with
      | Some source -> resolve (VRegSet.add current seen) source
      | None -> current
  in
  resolve VRegSet.empty register

let isNativeWrappingIntegerType = function
  | AST.TInt8 | AST.TInt16 | AST.TInt32 | AST.TInt64 | AST.TUInt8 | AST.TUInt16
  | AST.TUInt32 | AST.TUInt64 ->
      true
  | _ -> false

let isIncrementByOne typ inductionPhi nextValue = function
  | BinOp (dest, Add, Register source, Int64Const 1L, instructionType)
  | BinOp (dest, Add, Int64Const 1L, Register source, instructionType)
    when instructionType = typ ->
      dest = nextValue && source = inductionPhi
  | _ -> false

let registerInstrUsers (cfg : cfg) value =
  LabelMap.bindings cfg.blocks
  |> List.concat_map (fun (label, block) ->
      List.filter_map
        (fun instr ->
          if VRegSet.mem value (F.getInstrUses instr) then Some (label, instr)
          else None)
        block.instrs)

let terminatorUsesRegister (cfg : cfg) value =
  LabelMap.exists
    (fun _ block -> VRegSet.mem value (F.getTerminatorUses block.terminator))
    cfg.blocks

let isAffineOperandLoopInvariant preheader latch loopBlocks (cfg : cfg) typ =
  function
  | Register register -> (
      let definitions =
        LabelSet.elements loopBlocks
        |> List.concat_map (fun label ->
            match LabelMap.find_opt label cfg.blocks with
            | Some block -> block.instrs
            | None -> [])
        |> List.filter (fun instr -> F.getInstrDest instr = Some register)
      in
      match definitions with
      | [] -> true
      | [ Phi (destination, sources, Some phiType) ]
        when destination = register && phiType = typ -> (
          let initial =
            List.filter (fun (_, label) -> label = preheader) sources
          and backedge =
            List.filter (fun (_, label) -> label = latch) sources
          in
          match
            (sources, initial, backedge, LabelMap.find_opt latch cfg.blocks)
          with
          | [ _; _ ], [ _ ], [ (Register backedge, _) ], Some block ->
              resolveLatchCopy block.instrs backedge = register
          | _ -> false)
      | _ -> false)
  | Int64Const _ -> true
  | _ -> false

let affineOperandAtPreheader headerInstrs preheader typ operand =
  match operand with
  | Register register ->
      Option.value ~default:operand
        (List.find_map
           (function
             | Phi (destination, sources, Some phiType)
               when destination = register && phiType = typ ->
                 List.find_map
                   (fun (source, label) ->
                     if label = preheader then Some source else None)
                   sources
             | _ -> None)
           headerInstrs)
  | _ -> operand

let tryAffineExpression cfg latchLabel latch inductionPhi typ isLoopInvariant =
  let candidates =
    List.concat_map
      (fun scaleInstr ->
        let affine scaledValue coefficient =
          List.filter_map
            (fun affineInstr ->
              match affineInstr with
              | BinOp
                  ( affineValue,
                    ((Add | Sub) as offsetOperator),
                    Register scaledSource,
                    offset,
                    affineType )
                when scaledSource = scaledValue && affineType = typ
                     && isLoopInvariant offset ->
                  Some
                    ( scaledValue,
                      affineValue,
                      coefficient,
                      offset,
                      offsetOperator,
                      scaleInstr,
                      affineInstr )
              | BinOp
                  (affineValue, Add, offset, Register scaledSource, affineType)
                when scaledSource = scaledValue && affineType = typ
                     && isLoopInvariant offset ->
                  Some
                    ( scaledValue,
                      affineValue,
                      coefficient,
                      offset,
                      Add,
                      scaleInstr,
                      affineInstr )
              | _ -> None)
            latch.instrs
        in
        match scaleInstr with
        | BinOp
            (scaledValue, Shl, Register source, Int64Const 1L, instructionType)
          when source = inductionPhi && instructionType = typ ->
            affine scaledValue (Int64Const 2L)
        | BinOp (scaledValue, Mul, Register source, coefficient, instructionType)
          when source = inductionPhi && instructionType = typ
               && isLoopInvariant coefficient ->
            affine scaledValue coefficient
        | BinOp (scaledValue, Mul, coefficient, Register source, instructionType)
          when source = inductionPhi && instructionType = typ
               && isLoopInvariant coefficient ->
            affine scaledValue coefficient
        | _ -> [])
      latch.instrs
  in
  match candidates with
  | [
   ( scaledValue,
     affineValue,
     coefficient,
     offset,
     offsetOperator,
     scaleInstr,
     affineInstr );
  ] ->
      let scaledUsers = registerInstrUsers cfg scaledValue
      and affineUsers = registerInstrUsers cfg affineValue in
      let onlyLatch =
        affineUsers <> []
        && List.for_all (fun (label, _) -> label = latchLabel) affineUsers
      in
      if
        scaledUsers = [ (latchLabel, affineInstr) ]
        && onlyLatch
        && (not (terminatorUsesRegister cfg scaledValue))
        && not (terminatorUsesRegister cfg affineValue)
      then
        Some
          ( affineValue,
            coefficient,
            offset,
            offsetOperator,
            scaleInstr,
            affineInstr )
      else None
  | _ -> None

let tryAffineInductionCandidate (cfg : cfg) predecessors header loopBlocks =
  let preds =
    Option.value ~default:[] (LabelMap.find_opt header predecessors)
  in
  let outside =
    List.filter (fun label -> not (LabelSet.mem label loopBlocks)) preds
  and inside = List.filter (fun label -> LabelSet.mem label loopBlocks) preds in
  match (outside, inside, LabelMap.find_opt header cfg.blocks) with
  | [ preheader ], [ latchLabel ], Some headerBlock
    when LabelSet.equal loopBlocks (LabelSet.of_list [ header; latchLabel ])
    -> (
      match
        ( LabelMap.find_opt preheader cfg.blocks,
          LabelMap.find_opt latchLabel cfg.blocks )
      with
      | Some preheaderBlock, Some latch
        when preheaderBlock.terminator = Jump header
             && latch.terminator = Jump header -> (
          let candidates =
            List.filter_map
              (function
                | Phi (inductionPhi, sources, Some typ)
                  when isNativeWrappingIntegerType typ -> (
                    let initial =
                      List.filter
                        (fun (_, source) -> source = preheader)
                        sources
                    and backedge =
                      List.filter
                        (fun (_, source) -> source = latchLabel)
                        sources
                    in
                    match (sources, initial, backedge) with
                    | ( [ _; _ ],
                        [ (initialValue, _) ],
                        [ (Register nextValue, _) ] ) -> (
                        let resolvedNext =
                          resolveLatchCopy latch.instrs nextValue
                        in
                        let advances =
                          List.exists
                            (isIncrementByOne typ inductionPhi resolvedNext)
                            latch.instrs
                        in
                        let invariant =
                          isAffineOperandLoopInvariant preheader latchLabel
                            loopBlocks cfg typ
                        in
                        let affine =
                          tryAffineExpression cfg latchLabel latch inductionPhi
                            typ invariant
                        in
                        match (advances, affine) with
                        | ( true,
                            Some
                              ( affineValue,
                                coefficient,
                                offset,
                                offsetOperator,
                                scaleInstr,
                                affineInstr ) ) ->
                            Some
                              {
                                header;
                                preheader;
                                latch = latchLabel;
                                initialValue;
                                affineValue;
                                coefficient;
                                preheaderCoefficient =
                                  affineOperandAtPreheader headerBlock.instrs
                                    preheader typ coefficient;
                                _offset = offset;
                                preheaderOffset =
                                  affineOperandAtPreheader headerBlock.instrs
                                    preheader typ offset;
                                offsetOperator;
                                valueType = typ;
                                scaleInstr;
                                affineInstr;
                              }
                        | _ -> None)
                    | _ -> None)
                | _ -> None)
              headerBlock.instrs
          in
          match candidates with [ candidate ] -> Some candidate | _ -> None)
      | _ -> None)
  | _ -> None

let rec addPhiAfterPhis phi = function
  | (Phi _ as existing) :: rest -> existing :: addPhiAfterPhis phi rest
  | rest -> phi :: rest

let insertAfterLastUse value inserted instrs =
  let rec insert = function
    | [] -> ([], false)
    | instr :: rest ->
        let rest, already = insert rest in
        if already then (instr :: rest, true)
        else if VRegSet.mem value (F.getInstrUses instr) then
          (instr :: (inserted @ rest), true)
        else (instr :: rest, false)
  in
  let instrs, inserted = insert instrs in
  if inserted then instrs
  else Crash.crash "insertAfterLastUse: affine induction value has no latch use"

(*
Recognize a two-block native-width integer loop with an `i + 1` backedge and a unique
`a * i + b` use chain. `a` and `b` must be loop-invariant, so the preheader can
compute the initial value and the latch can advance the derived phi by `a` with
the same wrapping arithmetic. Reject extra scaled-value uses and non-canonical
control flow so the rewrite remains a local SSA substitution.
*)
let applyAffineInductionStrengthReductionWithTopology
    (topology : T.loopTopology) (cfg : cfg) =
  let candidate =
    LabelMap.bindings topology.T.loops
    |> List.find_map (fun (header, loopBlocks) ->
        tryAffineInductionCandidate cfg topology.T.predecessors header
          loopBlocks)
  in
  match candidate with
  | None -> (cfg, false)
  | Some candidate ->
      let fresh = nextRegisterId cfg in
      let initialScaleOperator, initialScaleOperand =
        match candidate.scaleInstr with
        | BinOp (_, Shl, _, _, typ) when typ = candidate.valueType ->
            (Shl, Int64Const 1L)
        | _ -> (Mul, candidate.preheaderCoefficient)
      in
      let initialScaled = VReg fresh
      and initialAffine = VReg (addInt fresh 1)
      and nextAffine = VReg (addInt fresh 2)
      and nextPhiSource = VReg (addInt fresh 3) in
      let preheaderInstrs =
        [
          BinOp
            ( initialScaled,
              initialScaleOperator,
              candidate.initialValue,
              initialScaleOperand,
              candidate.valueType );
          BinOp
            ( initialAffine,
              candidate.offsetOperator,
              Register initialScaled,
              candidate.preheaderOffset,
              candidate.valueType );
        ]
      in
      let derivedPhi =
        Phi
          ( candidate.affineValue,
            [
              (Register initialAffine, candidate.preheader);
              (Register nextPhiSource, candidate.latch);
            ],
            Some candidate.valueType )
      in
      let advance =
        BinOp
          ( nextAffine,
            Add,
            Register candidate.affineValue,
            candidate.coefficient,
            candidate.valueType )
      in
      let copy =
        Mov (nextPhiSource, Register nextAffine, Some candidate.valueType)
      in
      let blocks =
        LabelMap.mapi
          (fun label block ->
            if label = candidate.preheader then
              { block with instrs = block.instrs @ preheaderInstrs }
            else if label = candidate.header then
              { block with instrs = addPhiAfterPhis derivedPhi block.instrs }
            else if label = candidate.latch then
              {
                block with
                instrs =
                  block.instrs
                  |> List.filter (fun instr ->
                      instr <> candidate.scaleInstr
                      && instr <> candidate.affineInstr)
                  |> insertAfterLastUse candidate.affineValue [ advance; copy ];
              }
            else block)
          cfg.blocks
      in
      ({ cfg with blocks }, true)

let applyAffineInductionStrengthReduction cfg =
  match T.tryBuildLoopTopology cfg with
  | None -> (cfg, false)
  | Some topology ->
      applyAffineInductionStrengthReductionWithTopology topology cfg
