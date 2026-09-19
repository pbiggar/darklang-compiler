// LowerOwnershipVariants.fs - Preserve scheduled ownership clones and call routing in ANF.

module LowerOwnershipVariants

open ANF
open OwnedIR

type Lowered = {
    Functions: ANF.Function list
    Contracts: Map<AST.FunctionId, CallSignature>
    VarGen: ANF.VarGen
}

type LoweringError =
    | MissingSourceFunction of AST.FunctionId
    | MissingSourceCalls of AST.FunctionId * CallSiteIdentity list
    | InvalidOwnershipBoundary of AST.FunctionId

let private directTarget = function
    | Call (target, _) | BorrowedCall (target, _) | TailCall (target, _) -> Some target
    | _ -> None

let private replaceTarget replacement = function
    | Call (_, arguments) -> Call (replacement, arguments)
    | BorrowedCall (_, arguments) -> BorrowedCall (replacement, arguments)
    | TailCall (_, arguments) -> TailCall (replacement, arguments)
    | expression -> expression

let rec private ownedCalls (block: OwnedIR.Block<'leaf, 'id>) =
    block.Body.Operations
    |> List.collect (function
        | Evaluate (HIR.Call call) -> [call]
        | Evaluate (HIR.Branch (_, _, yes, no)) -> ownedCalls yes @ ownedCalls no
        | Evaluate (HIR.Leaf _ | HIR.ScalarBinding _) | Dup _ | Drop _ -> [])

let rec private rewriteAll targets expression =
    let rewriteCExpr cexpr =
        match directTarget cexpr with
        | Some target ->
            Map.tryFind target targets
            |> Option.map (fun replacement -> replaceTarget replacement cexpr)
            |> Option.defaultValue cexpr
        | None -> cexpr
    match expression with
    | Return _ | Jump _ -> expression
    | Let (id, cexpr, body) -> Let (id, rewriteCExpr cexpr, rewriteAll targets body)
    | If (condition, yes, no) -> If (condition, rewriteAll targets yes, rewriteAll targets no)
    | Join (parameter, continuation, entry) ->
        Join (parameter, rewriteAll targets continuation, rewriteAll targets entry)

let private rewriteSelected caller sites replacements expression =
    let rec rewrite pending expression =
        match expression with
        | Return _ | Jump _ -> expression, pending
        | Let (id, cexpr, body) ->
            let cexpr, afterCall =
                match directTarget cexpr, pending with
                | Some target, (site, expected) :: rest when target = expected ->
                    let rewritten =
                        Map.tryFind site replacements
                        |> Option.map (fun replacement -> replaceTarget replacement cexpr)
                        |> Option.defaultValue cexpr
                    rewritten, rest
                | _ -> cexpr, pending
            let body, remaining = rewrite afterCall body
            Let (id, cexpr, body), remaining
        | If (condition, yes, no) ->
            let yes, afterYes = rewrite pending yes
            let no, afterNo = rewrite afterYes no
            If (condition, yes, no), afterNo
        | Join (parameter, continuation, entry) ->
            // HIR structured branches enumerate the entry arms before their
            // continuation. ANF joins store the continuation first.
            let entry, afterEntry = rewrite pending entry
            let continuation, remaining = rewrite afterEntry continuation
            Join (parameter, continuation, entry), remaining
    let rewritten, remaining = rewrite sites expression
    match remaining with
    | [] -> Ok rewritten
    | rest -> Error (MissingSourceCalls (caller, rest |> List.map fst))

let private callSignature definition =
    VerifyOwnership.callSignatureOfFunction definition.Ownership
    |> Result.mapError (fun _ -> InvalidOwnershipBoundary definition.Definition.Id)

/// ANF keeps the ordinary runtime representation in this slice. The lowering
/// nevertheless makes every materialized symbol and ownership boundary
/// explicit so RC insertion, tail-call rewriting, and later storage lowering
/// share one authoritative contract registry.
let lower
    (originalOwned: OwnedIR.Function<'leaf, 'id> list)
    (plan: MaterializeOwnershipVariants.Plan<'leaf, 'id>)
    (originalANF: ANF.Function list)
    (varGen: ANF.VarGen)
    : Result<Lowered, LoweringError> =
    let anfById = originalANF |> List.map (fun functionDefinition -> functionDefinition.Id, functionDefinition) |> Map.ofList
    let replacements =
        MaterializeOwnershipVariants.rewrites plan
        |> List.map (fun rewrite -> rewrite.Site, rewrite.Specialized.Target)
        |> Map.ofList
    let callsByCaller =
        originalOwned
        |> List.map (fun definition ->
            definition.Definition.Id,
            (ownedCalls definition.Definition.Body
             |> List.map (fun call ->
                 { Caller = definition.Definition.Id; Result = call.Result.Id }, call.Target)))
        |> Map.ofList
    originalANF
    |> List.fold (fun result functionDefinition ->
        result |> Result.bind (fun rewritten ->
            let sites = Map.tryFind functionDefinition.Id callsByCaller |> Option.defaultValue []
            let hasSelectedCall =
                sites |> List.exists (fun (site, _) -> Map.containsKey site replacements)
            if not hasSelectedCall then Ok (functionDefinition :: rewritten)
            else
                rewriteSelected functionDefinition.Id sites replacements functionDefinition.Body
                |> Result.map (fun body -> { functionDefinition with Body = body } :: rewritten))) (Ok [])
    |> Result.map List.rev
    |> Result.bind (fun originals ->
        MaterializeOwnershipVariants.groups plan
        |> List.fold (fun result group ->
            result |> Result.bind (fun (clones, currentVarGen) ->
                let members = AST.NonEmptyList.toList group.Members
                let targets =
                    members
                    |> List.map (fun memberDefinition ->
                        memberDefinition.Original,
                        memberDefinition.Function.Definition.Id)
                    |> Map.ofList
                members
                |> List.fold (fun result memberDefinition ->
                    result |> Result.bind (fun (clones, currentVarGen) ->
                        match Map.tryFind memberDefinition.Original anfById with
                        | None -> Error (MissingSourceFunction memberDefinition.Original)
                        | Some source ->
                            let parameters, mapping, afterParameters =
                                source.TypedParams
                                |> List.fold (fun (parameters, mapping, current) parameter ->
                                    let fresh, next = ANF.freshVar current
                                    ({ parameter with Id = fresh } :: parameters,
                                     Map.add parameter.Id fresh mapping,
                                     next)) ([], Map.empty, currentVarGen)
                                |> fun (parameters, mapping, current) ->
                                    List.rev parameters, mapping, current
                            let renamedBody, nextVarGen =
                                ANF_Inlining.renameExpr mapping afterParameters source.Body
                            let clone = {
                                source with
                                    Id = memberDefinition.Function.Definition.Id
                                    Name = memberDefinition.Function.Definition.Name
                                    TypedParams = parameters
                                    Body = rewriteAll targets renamedBody
                            }
                            Ok (clone :: clones, nextVarGen))) (Ok (clones, currentVarGen)))) (Ok ([], varGen))
        |> Result.map (fun (clones, currentVarGen) -> List.rev clones, currentVarGen)
        |> Result.bind (fun (clones, finalVarGen) ->
            (MaterializeOwnershipVariants.groups plan
             |> List.collect (fun group ->
                 AST.NonEmptyList.toList group.Members
                 |> List.map (fun memberDefinition -> memberDefinition.Function)))
            |> List.fold (fun result definition ->
                result |> Result.bind (fun contracts ->
                    callSignature definition
                    |> Result.map (fun signature -> Map.add definition.Definition.Id signature contracts))) (Ok Map.empty)
            |> Result.map (fun contracts -> {
                Functions = originals @ clones
                Contracts = contracts
                VarGen = finalVarGen
            })))
