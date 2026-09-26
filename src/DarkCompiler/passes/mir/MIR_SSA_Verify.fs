// MIR_SSA_Verify.fs - Check the definition, dominance, and phi-edge invariants of SSA MIR.

module MIR_SSA_Verify

open MIR

type private Definition = { Block: Label; Position: int }

let private successors (block: BasicBlock) = SSA_Construction.getSuccessors block

let private reachableLabels (cfg: CFG) =
    let rec visit pending seen =
        match pending with
        | [] -> seen
        | label :: rest when Set.contains label seen -> visit rest seen
        | label :: rest ->
            match Map.tryFind label cfg.Blocks with
            | None -> visit rest seen
            | Some block -> visit (successors block @ rest) (Set.add label seen)
    visit [cfg.Entry] Set.empty

let private dominates (idoms: SSA_Construction.Dominators) entry source target =
    let rec ascend seen current =
        if current = source then true
        elif current = entry || Set.contains current seen then false
        else
            match Map.tryFind current idoms with
            | Some parent -> ascend (Set.add current seen) parent
            | None -> false
    ascend Set.empty target

let private firstError checks =
    checks |> List.tryPick (function Error error -> Some error | Ok () -> None)
    |> function Some error -> Error error | None -> Ok ()

let verifyFunction (func: Function) : Result<unit, string> =
    let cfg = func.CFG
    match Map.tryFind cfg.Entry cfg.Blocks with
    | None -> Error $"SSA MIR {func.Name}: missing entry block"
    | Some _ ->
        let reachable = reachableLabels cfg
        let predecessors = SSA_Construction.buildPredecessors cfg
        let dominators = SSA_Construction.computeDominators cfg predecessors
        let definitions =
            let parameterDefs =
                func.TypedParams
                |> List.map (fun parameter -> parameter.Reg, { Block = cfg.Entry; Position = -1 })
            let instructionDefs =
                cfg.Blocks
                |> Map.toList
                |> List.collect (fun (label, block) ->
                    block.Instrs
                    |> List.mapi (fun index instruction ->
                        MIROptimizationFacts.getInstrDest instruction
                        |> Option.map (fun reg ->
                            reg,
                            { Block = label
                              Position =
                                match instruction with
                                | Phi _ -> -1
                                | _ -> index }))
                    |> List.choose id)
            parameterDefs @ instructionDefs
        let duplicateDefinitions =
            definitions
            |> List.countBy fst
            |> List.tryFind (fun (_, count) -> count <> 1)
        match duplicateDefinitions with
        | Some (reg, _) -> Error $"SSA MIR {func.Name}: repeated definition of {reg}"
        | None ->
            let byRegister = definitions |> Map.ofList
            let checkUse label position reg =
                match Map.tryFind reg byRegister with
                | None -> Error $"SSA MIR {func.Name}: undefined use of {reg} in {label}"
                | Some definition when definition.Block = label && definition.Position < position -> Ok ()
                | Some definition when definition.Block <> label
                                       && dominates dominators cfg.Entry definition.Block label -> Ok ()
                | Some _ -> Error $"SSA MIR {func.Name}: {reg} does not dominate its use in {label}"
            let checkOperand label position operand =
                match operand with
                | Register reg -> checkUse label position reg
                | _ -> Ok ()
            let checkBlock label block =
                let edgeChecks =
                    successors block
                    |> List.map (fun target ->
                        if Map.containsKey target cfg.Blocks then Ok ()
                        else Error $"SSA MIR {func.Name}: missing target {target} from {label}")
                let instructionChecks =
                    block.Instrs
                    |> List.mapi (fun index instruction ->
                        match instruction with
                        | Phi (_, sources, _) ->
                            let expected =
                                Map.tryFind label predecessors
                                |> Option.defaultValue []
                                |> Set.ofList
                            let actual = sources |> List.map snd |> Set.ofList
                            let labelsValid =
                                if actual = expected && Set.count actual = List.length sources then Ok ()
                                else Error $"SSA MIR {func.Name}: phi edges disagree with predecessors of {label}"
                            let sourceChecks =
                                sources
                                |> List.map (fun (operand, predecessor) ->
                                    checkOperand predecessor System.Int32.MaxValue operand)
                            firstError (labelsValid :: sourceChecks)
                        | other ->
                            MIROptimizationFacts.foldInstrUses
                                (fun uses reg -> reg :: uses)
                                []
                                other
                            |> List.map (checkUse label index)
                            |> firstError)
                let terminatorUses =
                    MIROptimizationFacts.foldTerminatorUses
                        (fun uses reg -> reg :: uses)
                        []
                        block.Terminator
                    |> List.map (checkUse label System.Int32.MaxValue)
                firstError (edgeChecks @ instructionChecks @ terminatorUses)
            cfg.Blocks
            |> Map.toList
            |> List.filter (fun (label, _) -> Set.contains label reachable)
            |> List.map (fun (label, block) -> checkBlock label block)
            |> firstError
