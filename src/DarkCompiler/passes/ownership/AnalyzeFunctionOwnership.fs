// AnalyzeFunctionOwnership.fs - Schedule verified whole-function ownership over checked HIR.

module AnalyzeFunctionOwnership

open System.Diagnostics
open TypeRegistries
open LoweringPrimitives

type Context = {
    TypeReg: TypeRegistry
    TypeNames: TypeNameRegistry
    RecordFieldsReg: Map<string, (string * AST.SemanticType) list>
    RecordTypeParamsReg: Map<string, string list>
    VariantLookup: VariantLookup
    SumTypeNames: Set<string>
    RcSumShapeReg: MemoryModel.RcSumShapeRegistry
    FuncReg: FunctionRegistry
    FunctionNames: FunctionNameRegistry
    ModuleRegistry: AST.ModuleRegistry
}

type AnalysisError =
    | HIRConstructionFailed of ConstructHIRFunctions.ConstructionError
    | OwnershipElaborationFailed of ElaborateFunctionOwnership.ElaborationError
    | OwnedHIRVerificationFailed of VerifyOwnedHIR.VerificationError<HIR.ValueId>
    | FunctionOwnershipVerificationFailed of AST.FunctionId * OwnedIR.VerificationError<HIR.ValueId>
    | SpecializationSchedulingFailed of ScheduleOwnershipVariants.SchedulingError<HIR.ValueId>

type Analysis = private {
    Ownership: ElaborateFunctionOwnership.Analysis<ConstructHIRFunctions.Primitive>
    HIR: VerifyOwnedHIR.HIRContracts<ConstructHIRFunctions.Primitive>
    Schedule: ScheduleOwnershipVariants.Plan<ConstructHIRFunctions.Primitive, HIR.ValueId>
}

let functions analysis = ScheduleOwnershipVariants.functions analysis.Schedule
let semantics analysis =
    ScheduleOwnershipVariants.ownershipSemantics
        analysis.Schedule
        (ElaborateFunctionOwnership.semantics analysis.Ownership)
let hirContracts analysis =
    ScheduleOwnershipVariants.hirContracts analysis.Schedule analysis.HIR
let schedule analysis = analysis.Schedule
let originalFunctions analysis = ElaborateFunctionOwnership.functions analysis.Ownership

let private isManaged context (value: HIR.Value) =
    value.Type
    |> MemoryPlanning.rcShapeOfTypeWithSums
        context.RecordFieldsReg
        context.RecordTypeParamsReg
        context.RcSumShapeReg
    |> MemoryPlanning.rcShapeNeedsOwnedScopeRelease

let private callContract isManaged (call: HIR.FunctionCall) : HIR.PrimitiveContract = {
    Inputs = call.Arguments
    Operands = []
    Outputs = [{
        Value = call.Result
        Alias = if isManaged call.Result then HIR.UnknownManagedAlias else HIR.NoManagedAlias
    }]
    Effects = Set.singleton HIR.MayInvokeUserCode
}

let analyzeWithTrace
    (recordTiming: (string -> System.TimeSpan -> unit) option)
    (context: Context)
    (functions: CheckedAST.FunctionDef list)
    : Result<Analysis, AnalysisError> =
    let measure name operation =
        let timer = Stopwatch.StartNew()
        let result = operation ()
        timer.Stop()
        recordTiming
        |> Option.iter (fun record -> record name timer.Elapsed)
        result
    let infer types expression =
        LoweringTypeInference.inferTypeCore
            context.SumTypeNames
            context.TypeNames
            expression
            types
            context.TypeReg
            context.VariantLookup
            context.FuncReg
            context.FunctionNames
            context.ModuleRegistry
    let calls : ConstructHIRFunctions.CallContracts = {
        ExternalSignature = fun _ -> None
        Contract = fun _ -> Some (callContract (isManaged context))
    }
    measure
        "Ownership detail: HIR construction"
        (fun () ->
            ConstructHIRFunctions.constructFunctionsWithOpaqueFallback
                context.FunctionNames
                infer
                (fun expression -> ClosureAnalysis.freeVars expression Set.empty)
                calls
                functions)
    |> Result.mapError HIRConstructionFailed
    |> Result.bind (fun definitions ->
        let leafOwnership primitive =
            let contract = ConstructHIRFunctions.primitiveContract primitive
            ({
                Inputs =
                    match primitive with
                    | ConstructHIRFunctions.ListTransform (_, input, _) ->
                        contract.Inputs
                        |> List.choose (fun value ->
                            if value.Id = input.Id then Some (OwnedIR.Consumed input.Id)
                            elif isManaged context value then Some (OwnedIR.Borrowed value.Id)
                            else None)
                    | ConstructHIRFunctions.Literal _
                    | ConstructHIRFunctions.Unary _
                    | ConstructHIRFunctions.Binary _
                    | ConstructHIRFunctions.FreshManaged _ ->
                        contract.Inputs
                        |> List.choose (fun value ->
                            if isManaged context value then Some (OwnedIR.Borrowed value.Id) else None)
                Outputs =
                    contract.Outputs
                    |> List.choose (fun output ->
                        if isManaged context output.Value then Some output.Value.Id else None)
            } : OwnedIR.Contract<HIR.ValueId>)
        let dialect : ElaborateFunctionOwnership.Dialect<ConstructHIRFunctions.Primitive, ConstructHIRFunctions.Block> = {
            Body = ConstructHIRFunctions.body
            LeafOwnership = leafOwnership
            LeafUniqueness = fun primitive ->
                match primitive with
                | ConstructHIRFunctions.FreshManaged (output, _) -> {
                    RequiredInputs = Set.empty
                    UniqueOutputs = Set.singleton output.Id
                  }
                | ConstructHIRFunctions.ListTransform (output, _, _) -> {
                    RequiredInputs = Set.empty
                    UniqueOutputs = Set.singleton output.Id
                  }
                | ConstructHIRFunctions.Literal _
                | ConstructHIRFunctions.Unary _
                | ConstructHIRFunctions.Binary _ -> {
                    RequiredInputs = Set.empty
                    UniqueOutputs = Set.empty
                  }
            IsManaged = isManaged context
            ExternalCallOwnership = fun _ -> None
        }
        ElaborateFunctionOwnership.elaborateFunctionsWithTrace recordTiming dialect definitions
        |> Result.mapError OwnershipElaborationFailed
        |> Result.bind (fun analysis ->
            let hir : VerifyOwnedHIR.HIRContracts<ConstructHIRFunctions.Primitive> = {
                Leaf = ConstructHIRFunctions.primitiveContract
                CallSignature = fun _ -> None
                CallContract = fun call -> Some (callContract (isManaged context) call)
            }
            let ownedFunctions = ElaborateFunctionOwnership.functions analysis
            let ownership = ElaborateFunctionOwnership.semantics analysis
            measure
                "Ownership detail: Initial HIR and ownership verification"
                (fun () ->
                    VerifyOwnedHIR.verifyFunctions
                        hir
                        ownership
                        ownedFunctions)
            |> Result.mapError (fun error ->
                match error with
                | VerifyOwnedHIR.OwnershipVerificationFailed _ ->
                    ownedFunctions
                    |> List.tryPick (fun definition ->
                        match VerifyOwnership.verifyFunction ownership definition.Ownership definition.Definition.Body with
                        | Ok () -> None
                        | Error functionError ->
                            Some (FunctionOwnershipVerificationFailed (definition.Definition.Id, functionError)))
                    |> Option.defaultValue (OwnedHIRVerificationFailed error)
                | VerifyOwnedHIR.HIRVerificationFailed _ -> OwnedHIRVerificationFailed error)
            |> Result.bind (fun () ->
                let reservedSymbols =
                    context.FunctionNames |> Map.values |> Set.ofSeq
                measure
                    "Ownership detail: Specialization scheduling"
                    (fun () ->
                        ScheduleOwnershipVariants.scheduleWithTrace
                            recordTiming
                            ScheduleOwnershipVariants.defaultLimits
                            hir
                            ownership
                            reservedSymbols
                            ownedFunctions)
                |> Result.mapError SpecializationSchedulingFailed
                |> Result.map (fun scheduled -> {
                    Ownership = analysis
                    HIR = hir
                    Schedule = scheduled
                }))))

let analyze context functions =
    analyzeWithTrace None context functions
