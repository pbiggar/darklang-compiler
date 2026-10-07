(* MIRLoopInvariantMotion.ml - Build preheaders and hoist proven loop-invariant operations. *)
[@@@warning "-4"]

open MIR
module S = SSA_Construction
module F = MIROptimizationFacts
module T = MIRLoopTopology

(*
   Scalar results can move across loop iterations without changing ownership.
*)
let isScalarReturnType = MIRUnrolling.isScalarValueType

(*
   Check if an instruction is safe to hoist out of a loop.
*)
let isHoistableInstrWithEffectFreeCalls functions = function
  | BinOp _ | UnaryOp _ | HeapLoad _ | FloatSqrt _ | FloatAbs _ | FloatNeg _
  | Int64ToFloat _ | FloatToInt64 _ | FloatToBits _ ->
      true
  | Call (_, fn, _, _, typ) ->
      SpecializationIdentity.FunctionSet.mem fn functions
      && isScalarReturnType typ
  | _ -> false

let isHoistableInstr instr =
  isHoistableInstrWithEffectFreeCalls SpecializationIdentity.FunctionSet.empty
    instr

let nextInt value = Int32.to_int (Int32.add (Int32.of_int value) 1l)
let compareLabel (Label a) (Label b) = StringOrder.compare a b

let distinctLabels labels =
  let _, result =
    List.fold_left
      (fun (seen, result) label ->
        if LabelSet.mem label seen then (seen, result)
        else (LabelSet.add label seen, label :: result))
      (LabelSet.empty, []) labels
  in
  List.rev result

let loopDefs (cfg : cfg) blocks =
  LabelSet.fold
    (fun label defs ->
      match LabelMap.find_opt label cfg.blocks with
      | None -> defs
      | Some block ->
          List.fold_left
            (fun defs instr ->
              match F.getInstrDest instr with
              | Some dest -> VRegSet.add dest defs
              | None -> defs)
            defs block.instrs)
    blocks VRegSet.empty

(*
   Create preheaders only for reducible loops whose direct invariant work can use one.
   A header entered from several edges cannot receive LICM output directly: any
   hoisted definition would not dominate all entries.  This normalizes those entry
   edges to one block and preserves SSA by merging each header phi's outside values
   in a new preheader phi.  Existing simple preheaders are deliberately unchanged.
*)
let canonicalizeLoopPreheaders functions (topology : T.loopTopology) (cfg : cfg)
    =
  let fresh (cfg : cfg) (Label header) =
    let rec choose index =
      let suffix =
        if index = 0 then "preheader" else "preheader_" ^ string_of_int index
      in
      let candidate = Label (header ^ "_" ^ suffix) in
      if LabelMap.mem candidate cfg.blocks then choose (nextInt index)
      else candidate
    in
    choose 0
  in
  let rewriteTarget header preheader = function
    | Jump target when target = header -> Jump preheader
    | Branch (condition, yes, no) ->
        Branch
          ( condition,
            (if yes = header then preheader else yes),
            if no = header then preheader else no )
    | term -> term
  in
  let loops = topology.T.loops in
  let ordered =
    LabelMap.bindings loops
    |> List.sort (fun (header, blocks) (header', blocks') ->
        let order =
          Int.compare (LabelSet.cardinal blocks) (LabelSet.cardinal blocks')
        in
        if order = 0 then compareLabel header header' else order)
  in
  let cfg, _, changed =
    List.fold_left
      (fun (cfg, predecessors, changed) (header, blocks) ->
        let outside =
          Option.value ~default:[] (LabelMap.find_opt header predecessors)
          |> List.filter (fun pred -> not (LabelSet.mem pred blocks))
          |> distinctLabels |> List.sort compareLabel
        in
        let simple =
          match outside with
          | [ preheader ] -> (
              match LabelMap.find_opt preheader cfg.blocks with
              | Some block -> block.terminator = Jump header
              | None -> false)
          | _ -> false
        in
        let defs = loopDefs cfg blocks in
        let nested =
          LabelMap.fold
            (fun nestedHeader nestedLoop nested ->
              if nestedHeader <> header && LabelSet.subset nestedLoop blocks
              then LabelSet.union nested nestedLoop
              else nested)
            loops LabelSet.empty
        in
        let hasInvariant =
          LabelSet.exists
            (fun label ->
              match LabelMap.find_opt label cfg.blocks with
              | None -> false
              | Some block ->
                  List.exists
                    (fun instr ->
                      match F.getInstrDest instr with
                      | None -> false
                      | Some _ ->
                          isHoistableInstrWithEffectFreeCalls functions instr
                          && VRegSet.for_all
                               (fun used -> not (VRegSet.mem used defs))
                               (F.getInstrUses instr))
                    block.instrs)
            (LabelSet.diff blocks nested)
        in
        if outside = [] || simple || not hasInvariant then
          (cfg, predecessors, changed)
        else
          match LabelMap.find_opt header cfg.blocks with
          | None -> (cfg, predecessors, changed)
          | Some headerBlock ->
              let preheader = fresh cfg header in
              let prePhis, rewritten, _ =
                List.fold_left
                  (fun (prePhis, rewritten, reg) instr ->
                    match instr with
                    | Phi (dest, sources, typ) -> (
                        let outsideSources =
                          List.filter
                            (fun (_, source) -> List.mem source outside)
                            sources
                        and insideSources =
                          List.filter
                            (fun (_, source) -> not (List.mem source outside))
                            sources
                        in
                        match outsideSources with
                        | [] -> (prePhis, instr :: rewritten, reg)
                        | _ ->
                            let merged = VReg reg in
                            ( Phi (merged, outsideSources, typ) :: prePhis,
                              Phi
                                ( dest,
                                  (Register merged, preheader) :: insideSources,
                                  typ )
                              :: rewritten,
                              nextInt reg ))
                    | _ -> (prePhis, instr :: rewritten, reg))
                  ([], [], MIRInduction.nextRegisterId cfg)
                  headerBlock.instrs
              in
              let preBlock =
                {
                  label = preheader;
                  instrs = List.rev prePhis;
                  terminator = Jump header;
                }
              in
              let blocks =
                LabelMap.mapi
                  (fun label block ->
                    if List.mem label outside then
                      {
                        block with
                        terminator =
                          rewriteTarget header preheader block.terminator;
                      }
                    else if label = header then
                      { block with instrs = List.rev rewritten }
                    else block)
                  cfg.blocks
                |> LabelMap.add preheader preBlock
              in
              let cfg = { cfg with blocks } in
              (cfg, S.buildPredecessors cfg, true))
      (cfg, topology.T.predecessors, false)
      ordered
  in
  (cfg, changed)

let buildCopyMapForLicm (cfg : cfg) =
  let phiDests =
    LabelMap.fold
      (fun _ block dests ->
        List.fold_left
          (fun dests -> function
            | Phi (dest, _, _) -> VRegSet.add dest dests | _ -> dests)
          dests block.instrs)
      cfg.blocks VRegSet.empty
  in
  LabelMap.fold
    (fun _ block copies ->
      List.fold_left
        (fun copies -> function
          | Mov (dest, Register src, _) when dest <> src ->
              if VRegSet.mem dest phiDests || VRegMap.mem dest copies then
                copies
              else VRegMap.add dest src copies
          | _ -> copies)
        copies block.instrs)
    cfg.blocks VRegMap.empty

let resolveCopyForLicm copies operand =
  let rec resolve seen operand =
    match operand with
    | Register reg when not (VRegSet.mem reg seen) -> (
        match VRegMap.find_opt reg copies with
        | Some src -> resolve (VRegSet.add reg seen) (Register src)
        | None -> operand)
    | _ -> operand
  in
  resolve VRegSet.empty operand

let resolveInvariantOperand invariants operand =
  let rec resolve seen operand =
    match operand with
    | Register reg when not (VRegSet.mem reg seen) -> (
        match VRegMap.find_opt reg invariants with
        | Some source -> resolve (VRegSet.add reg seen) source
        | None -> operand)
    | _ -> operand
  in
  resolve VRegSet.empty operand

(*
   Apply loop-invariant code motion for loops with a simple preheader.
   Discovery can find a consumer in an earlier block on a later
   pass. Schedule the collected instructions by dependency before
   moving them together into the preheader.
*)
let applyLoopInvariantCodeMotionWithEffectFreeCalls functions
    (topology : T.loopTopology) (cfg : cfg) =
  let cfg, canonicalized = canonicalizeLoopPreheaders functions topology cfg in
  let topology =
    if canonicalized then
      match T.tryBuildLoopTopology cfg with
      | Some updated -> updated
      | None ->
          Crash.crash
            "LICM preheader canonicalization removed every reachable loop"
    else topology
  in
  let optimized, changed =
    LabelMap.fold
      (fun header blocks (cfg, changed) ->
        let copies = buildCopyMapForLicm cfg in
        let outside =
          Option.value ~default:[]
            (LabelMap.find_opt header topology.T.predecessors)
          |> List.filter (fun pred -> not (LabelSet.mem pred blocks))
        in
        let preheader =
          match outside with
          | [ label ] -> (
              match LabelMap.find_opt label cfg.blocks with
              | Some block when block.terminator = Jump header -> Some label
              | _ -> None)
          | _ -> None
        in
        match preheader with
        | None -> (cfg, changed)
        | Some preheader ->
            let defs = loopDefs cfg blocks in
            let order =
              header
              :: (LabelSet.elements (LabelSet.remove header blocks)
                 |> List.sort compareLabel)
            in
            let resolveOp = resolveCopyForLicm copies in
            let rec findInvariantPhis current =
              let next =
                LabelSet.fold
                  (fun label current ->
                    match LabelMap.find_opt label cfg.blocks with
                    | None -> current
                    | Some block ->
                        List.fold_left
                          (fun current instr ->
                            match instr with
                            | Phi (dest, sources, _) -> (
                                let sources =
                                  List.map
                                    (fun (op, label) ->
                                      ( resolveInvariantOperand current
                                          (resolveOp op),
                                        label ))
                                    sources
                                in
                                let outside =
                                  List.filter
                                    (fun (_, label) ->
                                      not (LabelSet.mem label blocks))
                                    sources
                                and inside =
                                  List.filter
                                    (fun (_, label) ->
                                      LabelSet.mem label blocks)
                                    sources
                                in
                                match outside with
                                | [] -> current
                                | (operand, _) :: rest ->
                                    if
                                      List.for_all
                                        (fun (op, _) -> op = operand)
                                        rest
                                    then
                                      let invariant =
                                        match operand with
                                        | Register reg ->
                                            (not (VRegSet.mem reg defs))
                                            || VRegMap.mem reg current
                                        | _ -> true
                                      in
                                      let insideOk =
                                        List.for_all
                                          (fun (op, _) ->
                                            match op with
                                            | Register reg when reg = dest ->
                                                true
                                            | _ -> op = operand)
                                          inside
                                      in
                                      if invariant && insideOk then
                                        VRegMap.add dest operand current
                                      else current
                                    else current)
                            | _ -> current)
                          current block.instrs)
                  blocks current
              in
              if VRegMap.equal ( = ) next current then current
              else findInvariantPhis next
            in
            let invariantPhis = findInvariantPhis VRegMap.empty in
            let invariantRegs =
              VRegSet.of_list (List.map fst (VRegMap.bindings invariantPhis))
            in
            let rewrite instr =
              let sub = resolveInvariantOperand invariantPhis in
              match instr with
              | BinOp (dest, op, a, b, typ) ->
                  BinOp (dest, op, sub a, sub b, typ)
              | UnaryOp (dest, op, src) -> UnaryOp (dest, op, sub src)
              | Call (dest, fn, args, types, typ) ->
                  Call (dest, fn, List.map sub args, types, typ)
              | HeapLoad (dest, addr, offset, typ) -> (
                  match sub (Register addr) with
                  | Register addr -> HeapLoad (dest, addr, offset, typ)
                  | _ ->
                      Crash.crash
                        "LICM: HeapLoad address should remain a register")
              | FloatSqrt (dest, src) -> FloatSqrt (dest, sub src)
              | FloatAbs (dest, src) -> FloatAbs (dest, sub src)
              | FloatNeg (dest, src) -> FloatNeg (dest, sub src)
              | Int64ToFloat (dest, src) -> Int64ToFloat (dest, sub src)
              | FloatToInt64 (dest, src) -> FloatToInt64 (dest, sub src)
              | FloatToBits (dest, src) -> FloatToBits (dest, sub src)
              | _ -> instr
            in
            let rec findHoistable invariants hoists =
              let invariants, hoists, changed =
                List.fold_left
                  (fun (invariants, hoists, changed) label ->
                    match LabelMap.find_opt label cfg.blocks with
                    | None -> (invariants, hoists, changed)
                    | Some block ->
                        let found, invariants, blockChanged =
                          List.fold_left
                            (fun (found, invariants, changed) instr ->
                              match F.getInstrDest instr with
                              | None -> (found, invariants, changed)
                              | Some dest ->
                                  let usesInvariant =
                                    VRegSet.for_all
                                      (fun reg ->
                                        (not (VRegSet.mem reg defs))
                                        || VRegSet.mem reg invariants)
                                      (F.getInstrUses instr)
                                  in
                                  if VRegSet.mem dest invariants then
                                    (found, invariants, changed)
                                  else if
                                    isHoistableInstrWithEffectFreeCalls
                                      functions instr
                                    && usesInvariant
                                  then
                                    ( found @ [ instr ],
                                      VRegSet.add dest invariants,
                                      true )
                                  else (found, invariants, changed))
                            ([], invariants, false) block.instrs
                        in
                        let hoists =
                          if found = [] then hoists
                          else
                            LabelMap.add label
                              (Option.value ~default:[]
                                 (LabelMap.find_opt label hoists)
                              @ found)
                              hoists
                        in
                        (invariants, hoists, changed || blockChanged))
                  (invariants, hoists, false)
                  order
              in
              if changed then findHoistable invariants hoists
              else (invariants, hoists)
            in
            let _, hoists = findHoistable invariantRegs LabelMap.empty in
            if LabelMap.is_empty hoists then (cfg, changed)
            else
              let rec orderHoists ordered pending =
                match pending with
                | [] -> List.rev ordered
                | _ -> (
                    let dests =
                      VRegSet.of_list (List.filter_map F.getInstrDest pending)
                    in
                    let selected =
                      List.mapi (fun index instr -> (index, instr)) pending
                      |> List.find_opt (fun (_, instr) ->
                          VRegSet.is_empty
                            (VRegSet.inter (F.getInstrUses instr) dests))
                    in
                    match selected with
                    | None ->
                        Crash.crash
                          "LICM found a cycle among hoisted instructions"
                    | Some (index, instr) ->
                        let remaining =
                          List.mapi (fun i instr -> (i, instr)) pending
                          |> List.filter_map (fun (i, instr) ->
                              if i = index then None else Some instr)
                        in
                        orderHoists (instr :: ordered) remaining)
              in
              let hoisted =
                List.concat_map
                  (fun label ->
                    Option.value ~default:[] (LabelMap.find_opt label hoists))
                  order
                |> orderHoists [] |> List.map rewrite
              in
              let blocks' =
                LabelMap.mapi
                  (fun label block ->
                    if label = preheader then
                      { block with instrs = block.instrs @ hoisted }
                    else if LabelSet.mem label blocks then
                      let dests =
                        Option.value ~default:[]
                          (LabelMap.find_opt label hoists)
                        |> List.filter_map F.getInstrDest
                        |> VRegSet.of_list
                      in
                      {
                        block with
                        instrs =
                          List.filter
                            (fun instr ->
                              match F.getInstrDest instr with
                              | Some dest -> not (VRegSet.mem dest dests)
                              | None -> true)
                            block.instrs;
                      }
                    else block)
                  cfg.blocks
              in
              ({ cfg with blocks = blocks' }, true))
      topology.T.loops (cfg, canonicalized)
  in
  (optimized, changed, topology)

let applyLoopInvariantCodeMotion cfg =
  match T.tryBuildLoopTopology cfg with
  | None -> (cfg, false)
  | Some topology ->
      let cfg, changed, _ =
        applyLoopInvariantCodeMotionWithEffectFreeCalls
          SpecializationIdentity.FunctionSet.empty topology cfg
      in
      (cfg, changed)
