// CodeGen.fs - Assemble planned function and runtime-helper instruction chunks.

module CodeGen_X86_64

open X64Operands
open X64Process
open X64CodeGenTypes
open X64ReleaseSelection
open X64FieldReferenceCounts
open X64ListReferenceCounts
open X64DictReferenceCounts
open X64ClosureReferenceCounts
open X64Functions

/// Reference-count runtime helpers required by the program's LIR instructions.
/// Keeping these requirements together lets code generation discover them in one pass.
type private RcHelperRequirements = {
    ListDecHelperLabels: Set<string>
    PlannedListDecHelpers: Map<string, int * MemoryModel.RcReleasePlan>
    PlannedDictDecHelpers: Map<string, MemoryModel.RcReleasePlan>
    DictDecHelperLabels: Set<string>
    NeedsListRcIncHelper: bool
    NeedsDictRcIncHelper: bool
    NeedsClosureRcIncHelper: bool
    NeedsClosureRcDecHelper: bool
    NeedsStreamRcDecHelper: bool
}

/// Translate a complete LIR program to x86-64 instructions
let translateProgram (LIR.Program (functions, variantRegistry, recordRegistry)) (enableLeakCheck: bool) : Result<X86_64.Instr list, string> =
    let sumShapeRegistry = rcSumShapeRegistryFromVariantRegistry variantRegistry
    let needsCliGetEnvHelper =
        functions
        |> List.exists (fun func ->
            func.CFG.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function
                    | LIR.CliNative (_, LIR.GetEnv, _) -> true
                    | _ -> false)))
    let needsCliExecuteHelper =
        functions
        |> List.exists (fun func ->
            func.CFG.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function
                    | LIR.CliNative (_, LIR.Execute, _) -> true
                    | _ -> false)))
    let needsCliRunProcessHelper =
        functions
        |> List.exists (fun func ->
            func.CFG.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function
                    | LIR.CliNative (_, LIR.RunProcess, _) -> true
                    | _ -> false)))
    let needsCliProcessLifecycleHelpers =
        functions
        |> List.exists (fun func ->
            func.CFG.Blocks
            |> Map.exists (fun _ block ->
                block.Instrs
                |> List.exists (function
                    | LIR.CliNative (_, (LIR.SpawnProcess | LIR.ProcessIO | LIR.TerminateProcess), _) -> true
                    | _ -> false)))

    let functionNames =
        functions
        |> List.map (fun func -> func.Id, func.Name)
        |> Map.ofList

    let closureCaptureTypes = closureCaptureTypesFromParams functions

    let unionLabelSets sets =
        match sets with
        | [] -> Set.empty
        | _ -> Set.unionMany sets

    let mergePlannedListDecHelperMaps
        (left: Map<string, int * MemoryModel.RcReleasePlan>)
        (right: Map<string, int * MemoryModel.RcReleasePlan>)
        : Map<string, int * MemoryModel.RcReleasePlan> =
        right
        |> Map.fold (fun acc helperLabel plannedHelper ->
            match Map.tryFind helperLabel acc with
            | Some existingHelper when existingHelper <> plannedHelper ->
                Crash.crash $"x64 planned list RefCountDec helper label collision for {helperLabel}"
            | Some _ ->
                acc
            | None ->
                Map.add helperLabel plannedHelper acc)
            left

    let unionPlannedListDecHelperMaps maps =
        maps
        |> List.fold mergePlannedListDecHelperMaps Map.empty

    let mergePlannedDictDecHelperMaps
        (left: Map<string, MemoryModel.RcReleasePlan>)
        (right: Map<string, MemoryModel.RcReleasePlan>)
        : Map<string, MemoryModel.RcReleasePlan> =
        right
        |> Map.fold (fun acc helperLabel releasePlan ->
            match Map.tryFind helperLabel acc with
            | Some existingPlan when existingPlan <> releasePlan ->
                Crash.crash $"x64 planned dict RefCountDec helper label collision for {helperLabel}"
            | Some _ ->
                acc
            | None ->
                Map.add helperLabel releasePlan acc)
            left

    let unionPlannedDictDecHelperMaps maps =
        maps
        |> List.fold mergePlannedDictDecHelperMaps Map.empty

    let rec listDecHelperLabelsInReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : Set<string> =
        let labelsInFieldReleases fieldReleases =
            fieldReleases
            |> List.map (function
                | MemoryModel.FieldRelease (_, fieldReleasePlan) ->
                    listDecHelperLabelsInReleasePlan fieldReleasePlan)
            |> unionLabelSets

        match releasePlan with
        | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
            Set.add
                (listDecHelperForReleasePlan releasePlan)
                (listDecHelperLabelsInReleasePlan elementRelease)
        | MemoryModel.RootRelease (_, _, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases))
        | MemoryModel.RootRelease (_, _, MemoryModel.BoxedSumPayloadRelease (_, fieldReleases, _))
        | MemoryModel.RootRelease (_, _, MemoryModel.ClosurePayloadRelease fieldReleases) ->
            labelsInFieldReleases fieldReleases
        | MemoryModel.RootRelease (_, _, MemoryModel.DictPayloadRelease (keyRelease, valueRelease)) ->
            Set.union
                (listDecHelperLabelsInReleasePlan keyRelease)
                (listDecHelperLabelsInReleasePlan valueRelease)
        | MemoryModel.RootRelease (_, _, MemoryModel.NoPayloadRelease) ->
            Set.empty
        | MemoryModel.NoReleasePlan
        | MemoryModel.DynamicBufferRelease _
        | MemoryModel.RecursiveRelease _ ->
            Set.empty

    let rec plannedListDecHelpersInReleasePlan
        (releasePlan: MemoryModel.RcReleasePlan)
        : Map<string, int * MemoryModel.RcReleasePlan> =
        let helpersInFieldReleases fieldReleases =
            fieldReleases
            |> List.map (function
                | MemoryModel.FieldRelease (_, fieldReleasePlan) ->
                    plannedListDecHelpersInReleasePlan fieldReleasePlan)
            |> unionPlannedListDecHelperMaps

        match releasePlan with
        | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
            let nestedHelpers =
                plannedListDecHelpersInReleasePlan elementRelease

            match elementRelease with
            | MemoryModel.RootRelease (payloadSize, (MemoryModel.GenericHeap | MemoryModel.StreamHeap), _) ->
                Map.empty
                |> Map.add
                    (plannedListDecHelperLabelForReleasePlan elementRelease)
                    (payloadSize, elementRelease)
                |> mergePlannedListDecHelperMaps nestedHelpers
            | MemoryModel.RecursiveRelease _ ->
                Map.empty
                |> Map.add
                    (plannedListDecHelperLabelForReleasePlan elementRelease)
                    (8, elementRelease)
                |> mergePlannedListDecHelperMaps nestedHelpers
            | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
                Map.empty
                |> Map.add
                    (plannedListDecHelperLabelForReleasePlan elementRelease)
                    (8, elementRelease)
                |> mergePlannedListDecHelperMaps nestedHelpers
            | _ ->
                nestedHelpers
        | MemoryModel.RootRelease (_, _, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases))
        | MemoryModel.RootRelease (_, _, MemoryModel.BoxedSumPayloadRelease (_, fieldReleases, _))
        | MemoryModel.RootRelease (_, _, MemoryModel.ClosurePayloadRelease fieldReleases) ->
            helpersInFieldReleases fieldReleases
        | MemoryModel.RootRelease (_, _, MemoryModel.DictPayloadRelease (keyRelease, valueRelease)) ->
            mergePlannedListDecHelperMaps
                (plannedListDecHelpersInReleasePlan keyRelease)
                (plannedListDecHelpersInReleasePlan valueRelease)
        | MemoryModel.RootRelease (_, _, MemoryModel.NoPayloadRelease) ->
            Map.empty
        | MemoryModel.NoReleasePlan
        | MemoryModel.DynamicBufferRelease _
        | MemoryModel.RecursiveRelease _ ->
            Map.empty

    let listDecHelperLabelsInType sourceType =
        sourceType
        |> tryRcReleasePlanOfType recordRegistry sumShapeRegistry
        |> Option.map listDecHelperLabelsInReleasePlan
        |> Option.defaultValue Set.empty

    let plannedListDecHelpersInType sourceType =
        sourceType
        |> tryRcReleasePlanOfType recordRegistry sumShapeRegistry
        |> Option.map plannedListDecHelpersInReleasePlan
        |> Option.defaultValue Map.empty

    let rec plannedDictDecHelpersInReleasePlan
        (releasePlan: MemoryModel.RcReleasePlan)
        : Map<string, MemoryModel.RcReleasePlan> =
        let helpersInFieldReleases fieldReleases =
            fieldReleases
            |> List.map (function
                | MemoryModel.FieldRelease (_, fieldReleasePlan) ->
                    plannedDictDecHelpersInReleasePlan fieldReleasePlan)
            |> unionPlannedDictDecHelperMaps

        match releasePlan with
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (keyRelease, valueRelease)) ->
            let self =
                if dictPayloadReleaseNeedsPlannedHelper keyRelease valueRelease then
                    Map.empty
                    |> Map.add (dictDecHelperForReleasePlan releasePlan) releasePlan
                else
                    Map.empty

            self
            |> mergePlannedDictDecHelperMaps (plannedDictDecHelpersInReleasePlan keyRelease)
            |> mergePlannedDictDecHelperMaps (plannedDictDecHelpersInReleasePlan valueRelease)
        | MemoryModel.RootRelease (_, nonDictKind, MemoryModel.DictPayloadRelease _) ->
            Crash.crash $"x64 planned dict dependency collection saw DictPayloadRelease for non-dict kind {nonDictKind}"
        | MemoryModel.RootRelease (_, _, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases))
        | MemoryModel.RootRelease (_, _, MemoryModel.BoxedSumPayloadRelease (_, fieldReleases, _))
        | MemoryModel.RootRelease (_, _, MemoryModel.ClosurePayloadRelease fieldReleases) ->
            helpersInFieldReleases fieldReleases
        | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
            plannedDictDecHelpersInReleasePlan elementRelease
        | MemoryModel.RootRelease (_, _, MemoryModel.NoPayloadRelease) ->
            Map.empty
        | MemoryModel.NoReleasePlan
        | MemoryModel.DynamicBufferRelease _
        | MemoryModel.RecursiveRelease _ ->
            Map.empty

    let plannedDictDecHelpersInType sourceType =
        sourceType
        |> tryRcReleasePlanOfType recordRegistry sumShapeRegistry
        |> Option.map plannedDictDecHelpersInReleasePlan
        |> Option.defaultValue Map.empty

    let rec dictDecHelperLabelsInReleasePlan (releasePlan: MemoryModel.RcReleasePlan) : Set<string> =
        let labelsInFieldReleases fieldReleases =
            fieldReleases
            |> List.map (function
                | MemoryModel.FieldRelease (_, fieldReleasePlan) ->
                    dictDecHelperLabelsInReleasePlan fieldReleasePlan)
            |> unionLabelSets

        match releasePlan with
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, _) ->
            Set.singleton (dictDecHelperForReleasePlan releasePlan)
        | MemoryModel.RootRelease (_, _, MemoryModel.FixedBlockPayloadRelease (_, fieldReleases))
        | MemoryModel.RootRelease (_, _, MemoryModel.BoxedSumPayloadRelease (_, fieldReleases, _))
        | MemoryModel.RootRelease (_, _, MemoryModel.ClosurePayloadRelease fieldReleases) ->
            labelsInFieldReleases fieldReleases
        | MemoryModel.RootRelease (_, _, MemoryModel.DictPayloadRelease (keyRelease, valueRelease)) ->
            Set.union
                (dictDecHelperLabelsInReleasePlan keyRelease)
                (dictDecHelperLabelsInReleasePlan valueRelease)
        | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
            dictDecHelperLabelsInReleasePlan elementRelease
        | MemoryModel.RootRelease (_, _, MemoryModel.NoPayloadRelease) ->
            Set.empty
        | MemoryModel.NoReleasePlan
        | MemoryModel.DynamicBufferRelease _
        | MemoryModel.RecursiveRelease _ ->
            Set.empty

    let dictDecHelperLabelsInType sourceType =
        sourceType
        |> tryRcReleasePlanOfType recordRegistry sumShapeRegistry
        |> Option.map dictDecHelperLabelsInReleasePlan
        |> Option.defaultValue Set.empty

    let emptyRcHelperRequirements = {
        ListDecHelperLabels = Set.empty
        PlannedListDecHelpers = Map.empty
        PlannedDictDecHelpers = Map.empty
        DictDecHelperLabels = Set.empty
        NeedsListRcIncHelper = false
        NeedsDictRcIncHelper = false
        NeedsClosureRcIncHelper = false
        NeedsClosureRcDecHelper = false
        NeedsStreamRcDecHelper = false
    }

    let collectRcHelperRequirementsFromInstr requirements instr =
        let collectReleasePlanRequirements releasePlan requirements =
            {
                requirements with
                    PlannedListDecHelpers =
                        mergePlannedListDecHelperMaps
                            requirements.PlannedListDecHelpers
                            (plannedListDecHelpersInReleasePlan releasePlan)
                    PlannedDictDecHelpers =
                        mergePlannedDictDecHelperMaps
                            requirements.PlannedDictDecHelpers
                            (plannedDictDecHelpersInReleasePlan releasePlan)
            }

        match instr with
        | LIR.RefCountDec (_, _, LIR.TaggedList, metadata) ->
            let releasePlan =
                requiredRcMetadataReleasePlan
                    "TaggedList RefCountDec helper selection"
                    metadata
            let withPlanRequirements =
                collectReleasePlanRequirements releasePlan requirements
            {
                withPlanRequirements with
                    ListDecHelperLabels =
                        Set.add
                            (listDecHelperForReleasePlan releasePlan)
                            withPlanRequirements.ListDecHelperLabels
            }
        | LIR.RefCountDec (_, _, LIR.DictHeap, metadata) ->
            let releasePlan =
                requiredRcMetadataReleasePlan
                    "DictHeap RefCountDec helper selection"
                    metadata
            let withPlanRequirements =
                collectReleasePlanRequirements releasePlan requirements
            {
                withPlanRequirements with
                    DictDecHelperLabels =
                        Set.add
                            (dictDecHelperForReleasePlan releasePlan)
                            withPlanRequirements.DictDecHelperLabels
            }
        | LIR.RefCountDec (_, _, ((LIR.GenericHeap | LIR.StreamHeap) as kind), metadata) ->
            let withReleasePlanRequirements =
                match rcMetadataReleasePlan metadata with
                | None ->
                    requirements
                | Some releasePlan ->
                    let withPlanRequirements =
                        collectReleasePlanRequirements releasePlan requirements
                    {
                        withPlanRequirements with
                            ListDecHelperLabels =
                                Set.union
                                    withPlanRequirements.ListDecHelperLabels
                                    (listDecHelperLabelsInReleasePlan releasePlan)
                            DictDecHelperLabels =
                                Set.union
                                    withPlanRequirements.DictDecHelperLabels
                                    (dictDecHelperLabelsInReleasePlan releasePlan)
                            NeedsClosureRcDecHelper =
                                withPlanRequirements.NeedsClosureRcDecHelper
                                || rcReleasePlanContains
                                    (releasePlanIsRootKind MemoryModel.ClosureHeap)
                                    releasePlan
                            NeedsStreamRcDecHelper =
                                withPlanRequirements.NeedsStreamRcDecHelper
                                || rcReleasePlanContains
                                    (releasePlanIsRootKind MemoryModel.StreamHeap)
                                    releasePlan
                    }
            if kind = LIR.StreamHeap then
                { withReleasePlanRequirements with NeedsStreamRcDecHelper = true }
            else
                withReleasePlanRequirements
        | LIR.RefCountDec (_, _, LIR.ClosureHeap, _) ->
            { requirements with NeedsClosureRcDecHelper = true }
        | LIR.RefCountInc (_, _, LIR.TaggedList, _) ->
            { requirements with NeedsListRcIncHelper = true }
        | LIR.RefCountInc (_, _, LIR.DictHeap, _) ->
            { requirements with NeedsDictRcIncHelper = true }
        | LIR.RefCountInc (_, _, LIR.ClosureHeap, _) ->
            { requirements with NeedsClosureRcIncHelper = true }
        | LIR.RawSlotInit (_, _, _, valueType) ->
            match slotInitRootRetainTarget recordRegistry sumShapeRegistry valueType with
            | Some SlotInitListRootRetain ->
                { requirements with NeedsListRcIncHelper = true }
            | Some SlotInitDictRootRetain ->
                { requirements with NeedsDictRcIncHelper = true }
            | Some SlotInitClosureRootRetain ->
                { requirements with NeedsClosureRcIncHelper = true }
            | Some SlotInitDynamicBufferRetain
            | Some (SlotInitGenericRootRetain _) ->
                requirements
            | None ->
                requirements
        | _ ->
            requirements

    let instructionRcHelperRequirements =
        functions
        |> List.fold (fun functionRequirements func ->
            func.CFG.Blocks
            |> Map.fold (fun blockRequirements _ block ->
                block.Instrs
                |> List.fold collectRcHelperRequirementsFromInstr blockRequirements)
                functionRequirements)
            emptyRcHelperRequirements

    let closureCaptureListDecHelperLabels =
        closureCaptureTypes
        |> Map.toList
        |> List.map (fun (_, captureTypes) ->
            captureTypes
            |> List.map listDecHelperLabelsInType
            |> unionLabelSets)
        |> unionLabelSets

    let closureCapturePlannedListDecHelpers =
        closureCaptureTypes
        |> Map.toList
        |> List.map (fun (_, captureTypes) ->
            captureTypes
            |> List.map plannedListDecHelpersInType
            |> unionPlannedListDecHelperMaps)
        |> unionPlannedListDecHelperMaps

    let closureCapturePlannedDictDecHelpers =
        closureCaptureTypes
        |> Map.toList
        |> List.map (fun (_, captureTypes) ->
            captureTypes
            |> List.map plannedDictDecHelpersInType
            |> unionPlannedDictDecHelperMaps)
        |> unionPlannedDictDecHelperMaps

    let closureCaptureDictDecHelperLabels =
        closureCaptureTypes
        |> Map.toList
        |> List.map (fun (_, captureTypes) ->
            captureTypes
            |> List.map dictDecHelperLabelsInType
            |> unionLabelSets)
        |> unionLabelSets

    let neededListDecHelperLabels =
        Set.union
            instructionRcHelperRequirements.ListDecHelperLabels
            closureCaptureListDecHelperLabels

    let neededPlannedListDecHelpers =
        mergePlannedListDecHelperMaps
            instructionRcHelperRequirements.PlannedListDecHelpers
            closureCapturePlannedListDecHelpers

    let neededPlannedDictDecHelpers =
        mergePlannedDictDecHelperMaps
            instructionRcHelperRequirements.PlannedDictDecHelpers
            closureCapturePlannedDictDecHelpers

    let plannedDictDecHelpersNeedListDecHelperLabels =
        neededPlannedDictDecHelpers
        |> Map.toList
        |> List.map (fun (_, releasePlan) -> listDecHelperLabelsInReleasePlan releasePlan)
        |> unionLabelSets

    let neededDictDecHelperLabels =
        Set.union
            instructionRcHelperRequirements.DictDecHelperLabels
            closureCaptureDictDecHelperLabels

    let typedDictDecHelpersNeedListDecHelper =
        if Set.contains dictRefCountDecListValueHelperLabel neededDictDecHelperLabels
           || Set.contains dictRefCountDecDictListValueHelperLabel neededDictDecHelperLabels
           || Set.contains dictRefCountDecTupleStringListValueHelperLabel neededDictDecHelperLabels
           || Set.contains dictRefCountDecTupleStringListDictValueHelperLabel neededDictDecHelperLabels
           || Set.contains dictRefCountDecDynamicKeyTupleStringListDictValueHelperLabel neededDictDecHelperLabels
           || Set.contains dictRefCountDecDynamicKeyListValueHelperLabel neededDictDecHelperLabels
           || Set.contains dictRefCountDecDynamicKeyDictListValueHelperLabel neededDictDecHelperLabels then
            Set.singleton listRefCountDecHelperLabel
        else
            Set.empty

    let typedListDecHelpersNeedListDecHelper =
        if Set.contains listRefCountDecDictListHelperLabel neededListDecHelperLabels then
            Set.singleton listRefCountDecHelperLabel
        else
            Set.empty

    let listHelperDependenciesForLabels (selectedLabels: Set<string>) : Set<string> =
        let staticDependencies =
            listRefCountDecHelperSpecs
            |> List.choose (fun (helperLabel, leafPayloadRelease) ->
                match leafPayloadRelease with
                | FixedBlockPlannedLeafPayload (_, releasePlan) when Set.contains helperLabel selectedLabels ->
                    Some (listDecHelperLabelsInReleasePlan releasePlan)
                | _ ->
                    None)
            |> unionLabelSets

        let plannedDependencies =
            neededPlannedListDecHelpers
            |> Map.toList
            |> List.choose (fun (helperLabel, (_, releasePlan)) ->
                if Set.contains helperLabel selectedLabels then
                    Some (listDecHelperLabelsInReleasePlan releasePlan)
                else
                    None)
            |> unionLabelSets

        Set.union staticDependencies plannedDependencies

    let rec closeListHelperDependencies (selectedLabels: Set<string>) : Set<string> =
        let nextLabels =
            Set.union selectedLabels (listHelperDependenciesForLabels selectedLabels)

        if nextLabels = selectedLabels then
            selectedLabels
        else
            closeListHelperDependencies nextLabels

    let selectedListDecHelperLabels =
        Set.unionMany
            [neededListDecHelperLabels
             plannedDictDecHelpersNeedListDecHelperLabels
             typedDictDecHelpersNeedListDecHelper
             typedListDecHelpersNeedListDecHelper]
        |> closeListHelperDependencies

    let selectedPlannedListDecHelpers =
        neededPlannedListDecHelpers
        |> Map.filter (fun helperLabel _ -> Set.contains helperLabel selectedListDecHelperLabels)

    let selectedPlannedListHelpersContain predicate =
        selectedPlannedListDecHelpers
        |> Map.exists (fun _ (_, releasePlan) -> rcReleasePlanContains predicate releasePlan)

    let selectedListHelpersNeedDictDecHelper =
        selectedListRefCountDecHelpersNeedDictDecHelper selectedListDecHelperLabels
        || selectedPlannedListHelpersContain (releasePlanIsRootKind MemoryModel.DictHeap)

    let selectedListHelpersNeedDictListValueDecHelper =
        selectedListRefCountDecHelpersNeedDictListValueDecHelper selectedListDecHelperLabels
        || selectedPlannedListHelpersContain releasePlanIsDictWithListValue

    let selectedListHelpersNeedClosureDecHelper =
        selectedListRefCountDecHelpersNeedClosureDecHelper selectedListDecHelperLabels
        || selectedPlannedListHelpersContain (releasePlanIsRootKind MemoryModel.ClosureHeap)

    let plannedDictHelpersNeedClosureDecHelper =
        neededPlannedDictDecHelpers
        |> Map.exists (fun _ releasePlan ->
            rcReleasePlanContains (releasePlanIsRootKind MemoryModel.ClosureHeap) releasePlan)

    let selectedListHelpersNeedStreamDecHelper =
        selectedPlannedListHelpersContain (releasePlanIsRootKind MemoryModel.StreamHeap)

    let plannedDictHelpersNeedStreamDecHelper =
        neededPlannedDictDecHelpers
        |> Map.exists (fun _ releasePlan ->
            rcReleasePlanContains (releasePlanIsRootKind MemoryModel.StreamHeap) releasePlan)

    let needsListRcIncHelper =
        instructionRcHelperRequirements.NeedsListRcIncHelper

    let needsDictRcIncHelper =
        instructionRcHelperRequirements.NeedsDictRcIncHelper

    let needsDictRcDecHelper =
        Set.contains dictRefCountDecHelperLabel neededDictDecHelperLabels

    let needsDictRcDecDynamicKeyHelper =
        Set.contains dictRefCountDecDynamicKeyHelperLabel neededDictDecHelperLabels

    let needsDictRcDecDynamicValueHelper =
        Set.contains dictRefCountDecDynamicValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecDynamicKeyValueHelper =
        Set.contains dictRefCountDecDynamicKeyValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecDynamicKeyListValueHelper =
        Set.contains dictRefCountDecDynamicKeyListValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecDynamicKeyDictValueHelper =
        Set.contains dictRefCountDecDynamicKeyDictValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecDynamicKeyDictListValueHelper =
        Set.contains dictRefCountDecDynamicKeyDictListValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecListValueHelper =
        Set.contains dictRefCountDecListValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecDictValueHelper =
        Set.contains dictRefCountDecDictValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecDictListValueHelper =
        Set.contains dictRefCountDecDictListValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecTupleStringListValueHelper =
        Set.contains dictRefCountDecTupleStringListValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecTupleStringListDictValueHelper =
        Set.contains dictRefCountDecTupleStringListDictValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecDynamicKeyTupleStringListDictValueHelper =
        Set.contains dictRefCountDecDynamicKeyTupleStringListDictValueHelperLabel neededDictDecHelperLabels

    let needsDictRcDecSumStringValueHelper =
        Set.contains dictRefCountDecSumStringValueHelperLabel neededDictDecHelperLabels

    let needsClosureRcIncHelper =
        instructionRcHelperRequirements.NeedsClosureRcIncHelper

    let needsClosureRcDecHelper =
        instructionRcHelperRequirements.NeedsClosureRcDecHelper

    let baseNeedsClosureRcDecHelper =
        needsClosureRcDecHelper
        || selectedListHelpersNeedClosureDecHelper
        || plannedDictHelpersNeedClosureDecHelper

    let selectedClosureHelpersNeedStreamDecHelper =
        if baseNeedsClosureRcDecHelper then
            closureCaptureTypes
            |> Map.exists (fun _ captureTypes ->
                captureTypes
                |> List.exists (fun captureType ->
                    tryRcReleasePlanOfType recordRegistry sumShapeRegistry captureType
                    |> Option.exists (rcReleasePlanContains (releasePlanIsRootKind MemoryModel.StreamHeap))))
        else
            false

    let needsStreamRcDecHelper =
        instructionRcHelperRequirements.NeedsStreamRcDecHelper
        || selectedListHelpersNeedStreamDecHelper
        || plannedDictHelpersNeedStreamDecHelper
        || selectedClosureHelpersNeedStreamDecHelper

    let emitClosureRcDecHelper =
        baseNeedsClosureRcDecHelper || needsStreamRcDecHelper

    let closurePayloadSizes =
        let allocationSizes =
            closurePayloadSizesFromAllocs functions
            |> Map.toList
            |> List.map (fun (funcId, payloadSize) ->
                match Map.tryFind funcId functionNames with
                | Some funcName -> funcName, payloadSize
                | None -> Crash.crash $"x64 metadata: missing closure target name for identity {AST.functionIdValue funcId}")
            |> Map.ofList
        Map.fold
            (fun acc funcName payloadSize -> Map.add funcName payloadSize acc)
            allocationSizes
            (closurePayloadSizesFromParams functions)
    let recursiveSumRcDecHelpers =
        recursiveReleaseTypesInFunctions functions
        |> Set.toList
        |> List.collect (generateRecursiveSumRefCountDecHelper enableLeakCheck recordRegistry sumShapeRegistry)
    let rec translateFuncs acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc |> List.concat)
        | func :: rest ->
            match translateFunction enableLeakCheck recordRegistry sumShapeRegistry functionNames func with
            | Error e -> Error e
            | Ok instrs -> translateFuncs (instrs :: acc) rest
    translateFuncs [] functions
    |> Result.map (fun allInstrs ->
        let allInstrs =
            if needsCliProcessLifecycleHelpers then
                allInstrs
                |> List.collect (fun instr ->
                    if instr = X86_64.Label "_epilogue__start" then
                        [instr; X86_64.CALL "__dark_cli_cleanup_processes"]
                    else
                        [instr])
            else
                allInstrs
        let listIncHelper =
            if needsListRcIncHelper then generateListRefCountIncHelper ()
            else []
        let listDecHelpers =
            generateNeededListRefCountDecHelpers
                selectedListDecHelperLabels
                selectedPlannedListDecHelpers
                enableLeakCheck
                recordRegistry
                sumShapeRegistry
        let dictIncHelper =
            if needsDictRcIncHelper then generateDictRefCountIncHelper ()
            else []
        let plannedDictDecHelpers =
            neededPlannedDictDecHelpers
            |> Map.toList
            |> List.collect (fun (helperLabel, releasePlan) ->
                generatePlannedDictRefCountDecHelper
                    helperLabel
                    releasePlan
                    enableLeakCheck
                    recordRegistry
                    sumShapeRegistry)
        let dictDecHelper =
            if needsDictRcDecHelper
               || selectedListHelpersNeedDictDecHelper
               || needsDictRcDecDictValueHelper
               || needsDictRcDecDynamicKeyDictValueHelper
               || needsDictRcDecTupleStringListDictValueHelper
               || needsDictRcDecDynamicKeyTupleStringListDictValueHelper
               || not (Map.isEmpty neededPlannedDictDecHelpers) then generateDictRefCountDecHelper dictRefCountDecHelperLabel MemoryModel.NoReleasePlan None false None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecDynamicKeyHelper =
            if needsDictRcDecDynamicKeyHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) None false None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecDynamicValueHelper =
            if needsDictRcDecDynamicValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicValueHelperLabel MemoryModel.NoReleasePlan (Some MemoryModel.DynamicStringBuffer) false None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecDynamicKeyValueHelper =
            if needsDictRcDecDynamicKeyValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyValueHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) (Some MemoryModel.DynamicStringBuffer) false None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecDynamicKeyListValueHelper =
            if needsDictRcDecDynamicKeyListValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyListValueHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) None true None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecDynamicKeyDictValueHelper =
            if needsDictRcDecDynamicKeyDictValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyDictValueHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) None false (Some dictRefCountDecHelperLabel) false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecDynamicKeyDictListValueHelper =
            if needsDictRcDecDynamicKeyDictListValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyDictListValueHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) None false (Some dictRefCountDecListValueHelperLabel) false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecListValueHelper =
            if needsDictRcDecListValueHelper || needsDictRcDecDictListValueHelper || needsDictRcDecDynamicKeyDictListValueHelper || selectedListHelpersNeedDictListValueDecHelper then generateDictRefCountDecHelper dictRefCountDecListValueHelperLabel MemoryModel.NoReleasePlan None true None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecDictValueHelper =
            if needsDictRcDecDictValueHelper then generateDictRefCountDecHelper dictRefCountDecDictValueHelperLabel MemoryModel.NoReleasePlan None false (Some dictRefCountDecHelperLabel) false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecDictListValueHelper =
            if needsDictRcDecDictListValueHelper then generateDictRefCountDecHelper dictRefCountDecDictListValueHelperLabel MemoryModel.NoReleasePlan None false (Some dictRefCountDecListValueHelperLabel) false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecTupleStringListValueHelper =
            if needsDictRcDecTupleStringListValueHelper then generateDictRefCountDecHelper dictRefCountDecTupleStringListValueHelperLabel MemoryModel.NoReleasePlan None false None false false (Some (16, dictTupleStringListValueReleasePlan)) enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecTupleStringListDictValueHelper =
            if needsDictRcDecTupleStringListDictValueHelper then generateDictRefCountDecHelper dictRefCountDecTupleStringListDictValueHelperLabel MemoryModel.NoReleasePlan None false None false false (Some (24, dictTupleStringListDictValueReleasePlan)) enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecDynamicKeyTupleStringListDictValueHelper =
            if needsDictRcDecDynamicKeyTupleStringListDictValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyTupleStringListDictValueHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) None false None false false (Some (24, dictTupleStringListDictValueReleasePlan)) enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let dictDecSumStringValueHelper =
            if needsDictRcDecSumStringValueHelper then generateDictRefCountDecHelper dictRefCountDecSumStringValueHelperLabel MemoryModel.NoReleasePlan None false None false false (Some (16, dictSumStringValueReleasePlan)) enableLeakCheck recordRegistry sumShapeRegistry
            else []
        let closureDecHelper =
            if emitClosureRcDecHelper then
                generateClosureRefCountDecHelper enableLeakCheck recordRegistry sumShapeRegistry closurePayloadSizes closureCaptureTypes
            else
                []
        let closureIncHelper =
            if needsClosureRcIncHelper then generateClosureRefCountIncHelper closurePayloadSizes
            else []
        let streamDecHelper =
            if needsStreamRcDecHelper then
                generateStreamRefCountDecHelper {
                    FunctionName = streamRefCountDecHelperLabel
                    StackSize = 0
                    UsedCalleeSaved = []
                    EnableLeakCheck = enableLeakCheck
                    RecordRegistry = recordRegistry
                    SumShapeRegistry = sumShapeRegistry
                    FunctionNames = Map.empty
                }
            else
                []
        allInstrs @ listIncHelper @ listDecHelpers @ dictIncHelper @ plannedDictDecHelpers @ dictDecHelper @ dictDecDynamicKeyHelper @ dictDecDynamicValueHelper @ dictDecDynamicKeyValueHelper @ dictDecDynamicKeyDictValueHelper @ dictDecDynamicKeyDictListValueHelper @ dictDecListValueHelper @ dictDecDictValueHelper @ dictDecDictListValueHelper @ dictDecTupleStringListValueHelper @ dictDecTupleStringListDictValueHelper @ dictDecDynamicKeyTupleStringListDictValueHelper @ dictDecSumStringValueHelper @ closureIncHelper @ closureDecHelper @ streamDecHelper @ recursiveSumRcDecHelpers @ generateCliArgvHelper enableLeakCheck @ generateCliEnvironmentPackedHelper enableLeakCheck @ generateCliDirectoryCurrentHelper enableLeakCheck @ generateCliSetEnvHelper enableLeakCheck @ generateCliUnsetEnvHelper enableLeakCheck @ generateCliDirectoryListHelper enableLeakCheck @ (if needsCliGetEnvHelper then generateCliGetEnvHelper enableLeakCheck else []) @ (if needsCliProcessLifecycleHelpers then generateLinuxCliSpawnProcessHelper () @ generateLinuxCliProcessLifecycleHelpers enableLeakCheck else []) @ (if needsCliRunProcessHelper then generateLinuxCliRunProcessHelper enableLeakCheck else []) @ (if needsCliExecuteHelper then generateLinuxCliExecuteHelper enableLeakCheck else []) @ genOomHandler () @ genRuntimeErrorHandler ())
