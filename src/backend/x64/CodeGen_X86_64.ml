[@@@warning "-4-42"]
module Map=struct
 include StringOrder.Map
 let toList=bindings
 let ofList=of_list
 let tryFind=find_opt
 let isEmpty=is_empty
 let fold f initial map=StringOrder.Map.fold (fun key value acc->f acc key value) map initial
end
module Set=struct
 include StringOrder.Set
 let contains=mem
 let unionMany xs=List.fold_left union empty xs
end
let foldBlocks f initial map=LIR.LabelMap.fold (fun key value acc->f acc key value) map initial
let defaultValue value option=Option.value option ~default:value
let kindName=function MemoryModel.GenericHeap->"GenericHeap"|MemoryModel.TaggedList->"TaggedList"|MemoryModel.DictHeap->"DictHeap"|MemoryModel.ClosureHeap->"ClosureHeap"|MemoryModel.StreamHeap->"StreamHeap"
(*  CodeGen_X86_64.ml - Assemble planned function and runtime-helper instruction chunks. *)
open X64Operands
open X64Process
open X64CodeGenTypes
open X64ReleaseSelection
open FieldReferenceCounts
open X64ListReferenceCounts
open X64DictReferenceCounts
open X64ClosureReferenceCounts
open X64Functions
(*  Reference-count runtime helpers required by the program's LIR instructions. *)
(*  Keeping these requirements together lets code generation discover them in one pass. *)
type rcHelperRequirements = {
    listDecHelperLabels: StringOrder.Set.t;
    plannedListDecHelpers: (int * MemoryModel.rcReleasePlan) StringOrder.Map.t;
    plannedDictDecHelpers: MemoryModel.rcReleasePlan StringOrder.Map.t;
    dictDecHelperLabels: StringOrder.Set.t;
    needsListRcIncHelper: bool;
    needsDictRcIncHelper: bool;
    needsClosureRcIncHelper: bool;
    needsClosureRcDecHelper: bool;
    needsStreamRcDecHelper: bool;
}
(*  Translate a complete LIR program to x86-64 instructions *)
let translateProgram (LIR.Program (functions, variantRegistry, recordRegistry)) (enableLeakCheck:bool) =
    let sumShapeRegistry = rcSumShapeRegistryFromVariantRegistry variantRegistry
    in
    let needsCliGetEnvHelper =
        functions
        |> List.exists (fun (func:LIR.functionDef) ->
            func.LIR.cfg.LIR.blocks
            |> LIR.LabelMap.exists (fun _ (block:LIR.basicBlock) ->
                block.LIR.instrs
                |> List.exists (function
                    | LIR.CliNative (_, LIR.GetEnv, _) -> true
                    | _ -> false)))
    in
    let needsCliExecuteHelper =
        functions
        |> List.exists (fun (func:LIR.functionDef) ->
            func.LIR.cfg.LIR.blocks
            |> LIR.LabelMap.exists (fun _ (block:LIR.basicBlock) ->
                block.LIR.instrs
                |> List.exists (function
                    | LIR.CliNative (_, LIR.Execute, _) -> true
                    | _ -> false)))
    in
    let needsCliRunProcessHelper =
        functions
        |> List.exists (fun (func:LIR.functionDef) ->
            func.LIR.cfg.LIR.blocks
            |> LIR.LabelMap.exists (fun _ (block:LIR.basicBlock) ->
                block.LIR.instrs
                |> List.exists (function
                    | LIR.CliNative (_, LIR.RunProcess, _) -> true
                    | _ -> false)))
    in
    let needsCliProcessLifecycleHelpers =
        functions
        |> List.exists (fun (func:LIR.functionDef) ->
            func.LIR.cfg.LIR.blocks
            |> LIR.LabelMap.exists (fun _ (block:LIR.basicBlock) ->
                block.LIR.instrs
                |> List.exists (function
                    | LIR.CliNative (_, (LIR.SpawnProcess | LIR.ProcessIO | LIR.TerminateProcess), _) -> true
                    | _ -> false)))
    in
    let functionNames =
        functions
        |> List.map (fun (func:LIR.functionDef) -> func.LIR.id, func.LIR.name)
        |> FunctionIdMap.ofList
    in
    let closureCaptureTypes = closureCaptureTypesFromParams functions
    in
    let unionLabelSets sets =
        match sets with
        | [] -> Set.empty
        | _ -> Set.unionMany sets
    in
    let mergePlannedListDecHelperMaps
        (left: (int * MemoryModel.rcReleasePlan) StringOrder.Map.t)
        (right: (int * MemoryModel.rcReleasePlan) StringOrder.Map.t)
        : (int * MemoryModel.rcReleasePlan) StringOrder.Map.t =
        right
        |> Map.fold (fun acc helperLabel plannedHelper ->
            match Map.tryFind helperLabel acc with
            | Some existingHelper when existingHelper <> plannedHelper ->
                Crash.crash ("x64 planned list RefCountDec helper label collision for "^helperLabel)
            | Some _ ->
                acc
            | None ->
                Map.add helperLabel plannedHelper acc)
            left
    in
    let unionPlannedListDecHelperMaps maps =
        maps
        |> List.fold_left mergePlannedListDecHelperMaps Map.empty
    in
    let mergePlannedDictDecHelperMaps
        (left: MemoryModel.rcReleasePlan StringOrder.Map.t)
        (right: MemoryModel.rcReleasePlan StringOrder.Map.t)
        : MemoryModel.rcReleasePlan StringOrder.Map.t =
        right
        |> Map.fold (fun acc helperLabel releasePlan ->
            match Map.tryFind helperLabel acc with
            | Some existingPlan when existingPlan <> releasePlan ->
                Crash.crash ("x64 planned dict RefCountDec helper label collision for "^helperLabel)
            | Some _ ->
                acc
            | None ->
                Map.add helperLabel releasePlan acc)
            left
    in
    let unionPlannedDictDecHelperMaps maps =
        maps
        |> List.fold_left mergePlannedDictDecHelperMaps Map.empty
    in
    let rec listDecHelperLabelsInReleasePlan (releasePlan: MemoryModel.rcReleasePlan) : StringOrder.Set.t =
        let labelsInFieldReleases fieldReleases =
            fieldReleases
            |> List.map (function
                | MemoryModel.FieldRelease (_, fieldReleasePlan) ->
                    listDecHelperLabelsInReleasePlan fieldReleasePlan)
            |> unionLabelSets
        in
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
    in
    let rec plannedListDecHelpersInReleasePlan
        (releasePlan: MemoryModel.rcReleasePlan)
        : (int * MemoryModel.rcReleasePlan) StringOrder.Map.t =
        let helpersInFieldReleases fieldReleases =
            fieldReleases
            |> List.map (function
                | MemoryModel.FieldRelease (_, fieldReleasePlan) ->
                    plannedListDecHelpersInReleasePlan fieldReleasePlan)
            |> unionPlannedListDecHelperMaps
        in
        match releasePlan with
        | MemoryModel.RootRelease (_, _, MemoryModel.TaggedListPayloadRelease elementRelease) ->
            let nestedHelpers =
                plannedListDecHelpersInReleasePlan elementRelease
            in
            (match elementRelease with
            | MemoryModel.RootRelease (payloadSize, (MemoryModel.GenericHeap | MemoryModel.StreamHeap), _) ->
                Map.empty
                |> Map.add
                    (plannedListDecHelperLabelForReleasePlan elementRelease)
                    (payloadSize, elementRelease)
                |> mergePlannedListDecHelperMaps nestedHelpers
            | MemoryModel.RootRelease (_, MemoryModel.TaggedList, _) ->
                Map.empty
                |> Map.add
                    (plannedListDecHelperLabelForReleasePlan elementRelease)
                    (8, elementRelease)
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
                nestedHelpers)
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
    in
    let listDecHelperLabelsInType sourceType =
        sourceType
        |> tryRcReleasePlanOfType recordRegistry sumShapeRegistry
        |> Option.map listDecHelperLabelsInReleasePlan
        |> defaultValue Set.empty
    in
    let plannedListDecHelpersInType sourceType =
        sourceType
        |> tryRcReleasePlanOfType recordRegistry sumShapeRegistry
        |> Option.map plannedListDecHelpersInReleasePlan
        |> defaultValue Map.empty
    in
    let rec plannedDictDecHelpersInReleasePlan
        (releasePlan: MemoryModel.rcReleasePlan)
        : MemoryModel.rcReleasePlan StringOrder.Map.t =
        let helpersInFieldReleases fieldReleases =
            fieldReleases
            |> List.map (function
                | MemoryModel.FieldRelease (_, fieldReleasePlan) ->
                    plannedDictDecHelpersInReleasePlan fieldReleasePlan)
            |> unionPlannedDictDecHelperMaps
        in
        match releasePlan with
        | MemoryModel.RootRelease (_, MemoryModel.DictHeap, MemoryModel.DictPayloadRelease (keyRelease, valueRelease)) ->
            let self =
                if dictPayloadReleaseNeedsPlannedHelper keyRelease valueRelease then
                    Map.empty
                    |> Map.add (dictDecHelperForReleasePlan releasePlan) releasePlan
                else
                    Map.empty
            in
            self
            |> mergePlannedDictDecHelperMaps (plannedDictDecHelpersInReleasePlan keyRelease)
            |> mergePlannedDictDecHelperMaps (plannedDictDecHelpersInReleasePlan valueRelease)
        | MemoryModel.RootRelease (_, nonDictKind, MemoryModel.DictPayloadRelease _) ->
            Crash.crash ("x64 planned dict dependency collection saw DictPayloadRelease for non-dict kind "^kindName nonDictKind)
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
    in
    let plannedDictDecHelpersInType sourceType =
        sourceType
        |> tryRcReleasePlanOfType recordRegistry sumShapeRegistry
        |> Option.map plannedDictDecHelpersInReleasePlan
        |> defaultValue Map.empty
    in
    let rec dictDecHelperLabelsInReleasePlan (releasePlan: MemoryModel.rcReleasePlan) : StringOrder.Set.t =
        let labelsInFieldReleases fieldReleases =
            fieldReleases
            |> List.map (function
                | MemoryModel.FieldRelease (_, fieldReleasePlan) ->
                    dictDecHelperLabelsInReleasePlan fieldReleasePlan)
            |> unionLabelSets
        in
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
    in
    let dictDecHelperLabelsInType sourceType =
        sourceType
        |> tryRcReleasePlanOfType recordRegistry sumShapeRegistry
        |> Option.map dictDecHelperLabelsInReleasePlan
        |> defaultValue Set.empty
    in
    let emptyRcHelperRequirements = {
        listDecHelperLabels = ( Set.empty
        );
        plannedListDecHelpers = ( Map.empty
        );
        plannedDictDecHelpers = ( Map.empty
        );
        dictDecHelperLabels = ( Set.empty
        );
        needsListRcIncHelper = ( false
        );
        needsDictRcIncHelper = ( false
        );
        needsClosureRcIncHelper = ( false
        );
        needsClosureRcDecHelper = ( false
        );
        needsStreamRcDecHelper = ( false
        )
    }
    in
    let collectRcHelperRequirementsFromInstr requirements instr =
        let collectReleasePlanRequirements releasePlan requirements =
            {
                requirements with
                    plannedListDecHelpers = (
                        mergePlannedListDecHelperMaps
                            requirements.plannedListDecHelpers
                            (plannedListDecHelpersInReleasePlan releasePlan)
                    );
                    plannedDictDecHelpers = (
                        mergePlannedDictDecHelperMaps
                            requirements.plannedDictDecHelpers
                            (plannedDictDecHelpersInReleasePlan releasePlan)
                    )
            }
        in
        match instr with
        | LIR.RefCountDec (_, _, LIR.TaggedList, metadata) ->
            let releasePlan =
                requiredRcMetadataReleasePlan
                    "TaggedList RefCountDec helper selection"
                    metadata
            in
            let withPlanRequirements =
                collectReleasePlanRequirements releasePlan requirements
            in
            {
                withPlanRequirements with
                    listDecHelperLabels = (
                        Set.add
                            (listDecHelperForReleasePlan releasePlan)
                            withPlanRequirements.listDecHelperLabels
                    )
            }
        | LIR.RefCountDec (_, _, LIR.DictHeap, metadata) ->
            let releasePlan =
                requiredRcMetadataReleasePlan
                    "DictHeap RefCountDec helper selection"
                    metadata
            in
            let withPlanRequirements =
                collectReleasePlanRequirements releasePlan requirements
            in
            {
                withPlanRequirements with
                    dictDecHelperLabels = (
                        Set.add
                            (dictDecHelperForReleasePlan releasePlan)
                            withPlanRequirements.dictDecHelperLabels
                    )
            }
        | LIR.RefCountDec (_, _, ((LIR.GenericHeap | LIR.StreamHeap) as kind), metadata) ->
            let withReleasePlanRequirements =
                match rcMetadataReleasePlan metadata with
                | None ->
                    requirements
                | Some releasePlan ->
                    let withPlanRequirements =
                        collectReleasePlanRequirements releasePlan requirements
                    in
                    {
                        withPlanRequirements with
                            listDecHelperLabels = (
                                Set.union
                                    withPlanRequirements.listDecHelperLabels
                                    (listDecHelperLabelsInReleasePlan releasePlan)
                            );
                            dictDecHelperLabels = (
                                Set.union
                                    withPlanRequirements.dictDecHelperLabels
                                    (dictDecHelperLabelsInReleasePlan releasePlan)
                            );
                            needsClosureRcDecHelper = (
                                withPlanRequirements.needsClosureRcDecHelper
                                || rcReleasePlanContains
                                    (releasePlanIsRootKind MemoryModel.ClosureHeap)
                                    releasePlan
                            );
                            needsStreamRcDecHelper = (
                                withPlanRequirements.needsStreamRcDecHelper
                                || rcReleasePlanContains
                                    (releasePlanIsRootKind MemoryModel.StreamHeap)
                                    releasePlan
                            )
                    }
            in
            if kind = LIR.StreamHeap then
                { withReleasePlanRequirements with needsStreamRcDecHelper = true }
            else
                withReleasePlanRequirements
        | LIR.RefCountDec (_, _, LIR.ClosureHeap, _) ->
            { requirements with needsClosureRcDecHelper = true }
        | LIR.RefCountInc (_, _, LIR.TaggedList, _) ->
            { requirements with needsListRcIncHelper = true }
        | LIR.RefCountInc (_, _, LIR.DictHeap, _) ->
            { requirements with needsDictRcIncHelper = true }
        | LIR.RefCountInc (_, _, LIR.ClosureHeap, _) ->
            { requirements with needsClosureRcIncHelper = true }
        | LIR.RawSlotInit (_, _, _, valueType) ->
            (match slotInitRootRetainTarget recordRegistry sumShapeRegistry valueType with
            | Some SlotInitListRootRetain ->
                { requirements with needsListRcIncHelper = true }
            | Some SlotInitDictRootRetain ->
                { requirements with needsDictRcIncHelper = true }
            | Some SlotInitClosureRootRetain ->
                { requirements with needsClosureRcIncHelper = true }
            | Some SlotInitDynamicBufferRetain
            | Some (SlotInitGenericRootRetain _) ->
                requirements
            | None ->
                requirements)
        | _ ->
            requirements
    in
    let instructionRcHelperRequirements =
        functions
        |> List.fold_left (fun functionRequirements (func:LIR.functionDef) ->
            func.LIR.cfg.LIR.blocks
            |> foldBlocks (fun blockRequirements _ (block:LIR.basicBlock) ->
                block.LIR.instrs
                |> List.fold_left collectRcHelperRequirementsFromInstr blockRequirements)
                functionRequirements)
            emptyRcHelperRequirements
    in
    let closureCaptureListDecHelperLabels =
        closureCaptureTypes
        |> Map.toList
        |> List.map (fun (_, captureTypes) ->
            captureTypes
            |> List.map listDecHelperLabelsInType
            |> unionLabelSets)
        |> unionLabelSets
    in
    let closureCapturePlannedListDecHelpers =
        closureCaptureTypes
        |> Map.toList
        |> List.map (fun (_, captureTypes) ->
            captureTypes
            |> List.map plannedListDecHelpersInType
            |> unionPlannedListDecHelperMaps)
        |> unionPlannedListDecHelperMaps
    in
    let closureCapturePlannedDictDecHelpers =
        closureCaptureTypes
        |> Map.toList
        |> List.map (fun (_, captureTypes) ->
            captureTypes
            |> List.map plannedDictDecHelpersInType
            |> unionPlannedDictDecHelperMaps)
        |> unionPlannedDictDecHelperMaps
    in
    let closureCaptureDictDecHelperLabels =
        closureCaptureTypes
        |> Map.toList
        |> List.map (fun (_, captureTypes) ->
            captureTypes
            |> List.map dictDecHelperLabelsInType
            |> unionLabelSets)
        |> unionLabelSets
    in
    let neededListDecHelperLabels =
        Set.union
            instructionRcHelperRequirements.listDecHelperLabels
            closureCaptureListDecHelperLabels
    in
    let neededPlannedListDecHelpers =
        mergePlannedListDecHelperMaps
            instructionRcHelperRequirements.plannedListDecHelpers
            closureCapturePlannedListDecHelpers
    in
    let neededPlannedDictDecHelpers =
        mergePlannedDictDecHelperMaps
            instructionRcHelperRequirements.plannedDictDecHelpers
            closureCapturePlannedDictDecHelpers
    in
    let plannedDictDecHelpersNeedListDecHelperLabels =
        neededPlannedDictDecHelpers
        |> Map.toList
        |> List.map (fun (_, releasePlan) -> listDecHelperLabelsInReleasePlan releasePlan)
        |> unionLabelSets
    in
    let neededDictDecHelperLabels =
        Set.union
            instructionRcHelperRequirements.dictDecHelperLabels
            closureCaptureDictDecHelperLabels
    in
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
    in
    let typedListDecHelpersNeedListDecHelper =
        if Set.contains listRefCountDecDictListHelperLabel neededListDecHelperLabels then
            Set.singleton listRefCountDecHelperLabel
        else
            Set.empty
    in
    let listHelperDependenciesForLabels (selectedLabels: StringOrder.Set.t) : StringOrder.Set.t =
        let staticDependencies =
            listRefCountDecHelperSpecs
            |> List.filter_map (fun (helperLabel, leafPayloadRelease) ->
                match leafPayloadRelease with
                | FixedBlockPlannedLeafPayload (_, releasePlan) when Set.contains helperLabel selectedLabels ->
                    Some (listDecHelperLabelsInReleasePlan releasePlan)
                | _ ->
                    None)
            |> unionLabelSets
        in
        let plannedDependencies =
            neededPlannedListDecHelpers
            |> Map.toList
            |> List.filter_map (fun (helperLabel, (_, releasePlan)) ->
                if Set.contains helperLabel selectedLabels then
                    Some (listDecHelperLabelsInReleasePlan releasePlan)
                else
                    None)
            |> unionLabelSets
        in
        Set.union staticDependencies plannedDependencies
    in
    let rec closeListHelperDependencies (selectedLabels: StringOrder.Set.t) : StringOrder.Set.t =
        let nextLabels =
            Set.union selectedLabels (listHelperDependenciesForLabels selectedLabels)
        in
        if Set.equal nextLabels selectedLabels then
            selectedLabels
        else
            closeListHelperDependencies nextLabels
    in
    let selectedListDecHelperLabels =
        Set.unionMany
            [neededListDecHelperLabels;
             plannedDictDecHelpersNeedListDecHelperLabels;
             typedDictDecHelpersNeedListDecHelper;
             typedListDecHelpersNeedListDecHelper]
        |> closeListHelperDependencies
    in
    let selectedPlannedListDecHelpers =
        neededPlannedListDecHelpers
        |> Map.filter (fun helperLabel _ -> Set.contains helperLabel selectedListDecHelperLabels)
    in
    let selectedPlannedListHelpersContain predicate =
        selectedPlannedListDecHelpers
        |> Map.exists (fun _ (_, releasePlan) -> rcReleasePlanContains predicate releasePlan)
    in
    let selectedListHelpersNeedDictDecHelper =
        selectedListRefCountDecHelpersNeedDictDecHelper selectedListDecHelperLabels
        || selectedPlannedListHelpersContain (releasePlanIsRootKind MemoryModel.DictHeap)
    in
    let selectedListHelpersNeedDictListValueDecHelper =
        selectedListRefCountDecHelpersNeedDictListValueDecHelper selectedListDecHelperLabels
        || selectedPlannedListHelpersContain releasePlanIsDictWithListValue
    in
    let selectedListHelpersNeedClosureDecHelper =
        selectedListRefCountDecHelpersNeedClosureDecHelper selectedListDecHelperLabels
        || selectedPlannedListHelpersContain (releasePlanIsRootKind MemoryModel.ClosureHeap)
    in
    let plannedDictHelpersNeedClosureDecHelper =
        neededPlannedDictDecHelpers
        |> Map.exists (fun _ releasePlan ->
            rcReleasePlanContains (releasePlanIsRootKind MemoryModel.ClosureHeap) releasePlan)
    in
    let selectedListHelpersNeedStreamDecHelper =
        selectedPlannedListHelpersContain (releasePlanIsRootKind MemoryModel.StreamHeap)
    in
    let plannedDictHelpersNeedStreamDecHelper =
        neededPlannedDictDecHelpers
        |> Map.exists (fun _ releasePlan ->
            rcReleasePlanContains (releasePlanIsRootKind MemoryModel.StreamHeap) releasePlan)
    in
    let needsListRcIncHelper =
        instructionRcHelperRequirements.needsListRcIncHelper
    in
    let needsDictRcIncHelper =
        instructionRcHelperRequirements.needsDictRcIncHelper
    in
    let needsDictRcDecHelper =
        Set.contains dictRefCountDecHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecDynamicKeyHelper =
        Set.contains dictRefCountDecDynamicKeyHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecDynamicValueHelper =
        Set.contains dictRefCountDecDynamicValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecDynamicKeyValueHelper =
        Set.contains dictRefCountDecDynamicKeyValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecDynamicKeyListValueHelper =
        Set.contains dictRefCountDecDynamicKeyListValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecDynamicKeyDictValueHelper =
        Set.contains dictRefCountDecDynamicKeyDictValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecDynamicKeyDictListValueHelper =
        Set.contains dictRefCountDecDynamicKeyDictListValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecListValueHelper =
        Set.contains dictRefCountDecListValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecDictValueHelper =
        Set.contains dictRefCountDecDictValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecDictListValueHelper =
        Set.contains dictRefCountDecDictListValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecTupleStringListValueHelper =
        Set.contains dictRefCountDecTupleStringListValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecTupleStringListDictValueHelper =
        Set.contains dictRefCountDecTupleStringListDictValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecDynamicKeyTupleStringListDictValueHelper =
        Set.contains dictRefCountDecDynamicKeyTupleStringListDictValueHelperLabel neededDictDecHelperLabels
    in
    let needsDictRcDecSumStringValueHelper =
        Set.contains dictRefCountDecSumStringValueHelperLabel neededDictDecHelperLabels
    in
    let needsClosureRcIncHelper =
        instructionRcHelperRequirements.needsClosureRcIncHelper
    in
    let needsClosureRcDecHelper =
        instructionRcHelperRequirements.needsClosureRcDecHelper
    in
    let baseNeedsClosureRcDecHelper =
        needsClosureRcDecHelper
        || selectedListHelpersNeedClosureDecHelper
        || plannedDictHelpersNeedClosureDecHelper
    in
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
    in
    let needsStreamRcDecHelper =
        instructionRcHelperRequirements.needsStreamRcDecHelper
        || selectedListHelpersNeedStreamDecHelper
        || plannedDictHelpersNeedStreamDecHelper
        || selectedClosureHelpersNeedStreamDecHelper
    in
    let emitClosureRcDecHelper =
        baseNeedsClosureRcDecHelper || needsStreamRcDecHelper
    in
    let closurePayloadSizes =
        let allocationSizes =
            closurePayloadSizesFromAllocs functions
            |> FunctionIdMap.toList
            |> List.map (fun (funcId, payloadSize) ->
                match FunctionIdMap.tryFind funcId functionNames with
                | Some funcName -> funcName, payloadSize
                | None -> Crash.crash (Printf.sprintf "x64 metadata: missing closure target name for identity %Lu" (AST.functionIdValue funcId)))
            |> Map.ofList
        in
        Map.fold
            (fun acc funcName payloadSize -> Map.add funcName payloadSize acc)
            allocationSizes
            (closurePayloadSizesFromParams functions)
    in
    let recursiveNominalRcDecHelpers =
        recursiveReleaseTypesInFunctions functions
        |> MemoryPlanning.SemanticTypeSet.elements
        |> List.concat_map (generateRecursiveNominalRefCountDecHelper enableLeakCheck recordRegistry sumShapeRegistry)
    in
    let rec translateFuncs acc remaining =
        match remaining with
        | [] -> Ok (List.rev acc |> List.concat)
        | func :: rest ->
            (match translateFunction enableLeakCheck recordRegistry sumShapeRegistry functionNames func with
            | Error e -> Error e
            | Ok instrs -> translateFuncs (instrs :: acc) rest)
    in
    translateFuncs [] functions
    |> Result.map (fun allInstrs ->
        let allInstrs =
            if needsCliProcessLifecycleHelpers then
                allInstrs
                |> List.concat_map (fun instr ->
                    if instr = X86_64.Label "_epilogue__start" then
                        [instr; X86_64.CALL "__dark_cli_cleanup_processes"]
                    else
                        [instr])
            else
                allInstrs
        in
        let listIncHelper =
            if needsListRcIncHelper then generateListRefCountIncHelper ()
            else []
        in
        let listDecHelpers =
            generateNeededListRefCountDecHelpers
                selectedListDecHelperLabels
                selectedPlannedListDecHelpers
                enableLeakCheck
                recordRegistry
                sumShapeRegistry
        in
        let dictIncHelper =
            if needsDictRcIncHelper then generateDictRefCountIncHelper ()
            else []
        in
        let plannedDictDecHelpers =
            neededPlannedDictDecHelpers
            |> Map.toList
            |> List.concat_map (fun (helperLabel, releasePlan) ->
                generatePlannedDictRefCountDecHelper
                    helperLabel
                    releasePlan
                    enableLeakCheck
                    recordRegistry
                    sumShapeRegistry)
        in
        let dictDecHelper =
            if needsDictRcDecHelper
               || selectedListHelpersNeedDictDecHelper
               || needsDictRcDecDictValueHelper
               || needsDictRcDecDynamicKeyDictValueHelper
               || needsDictRcDecTupleStringListDictValueHelper
               || needsDictRcDecDynamicKeyTupleStringListDictValueHelper
               || not (Map.isEmpty neededPlannedDictDecHelpers) then generateDictRefCountDecHelper dictRefCountDecHelperLabel MemoryModel.NoReleasePlan None false None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecDynamicKeyHelper =
            if needsDictRcDecDynamicKeyHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) None false None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecDynamicValueHelper =
            if needsDictRcDecDynamicValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicValueHelperLabel MemoryModel.NoReleasePlan (Some MemoryModel.DynamicStringBuffer) false None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecDynamicKeyValueHelper =
            if needsDictRcDecDynamicKeyValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyValueHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) (Some MemoryModel.DynamicStringBuffer) false None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let _dictDecDynamicKeyListValueHelper =
            if needsDictRcDecDynamicKeyListValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyListValueHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) None true None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecDynamicKeyDictValueHelper =
            if needsDictRcDecDynamicKeyDictValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyDictValueHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) None false (Some dictRefCountDecHelperLabel) false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecDynamicKeyDictListValueHelper =
            if needsDictRcDecDynamicKeyDictListValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyDictListValueHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) None false (Some dictRefCountDecListValueHelperLabel) false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecListValueHelper =
            if needsDictRcDecListValueHelper || needsDictRcDecDictListValueHelper || needsDictRcDecDynamicKeyDictListValueHelper || selectedListHelpersNeedDictListValueDecHelper then generateDictRefCountDecHelper dictRefCountDecListValueHelperLabel MemoryModel.NoReleasePlan None true None false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecDictValueHelper =
            if needsDictRcDecDictValueHelper then generateDictRefCountDecHelper dictRefCountDecDictValueHelperLabel MemoryModel.NoReleasePlan None false (Some dictRefCountDecHelperLabel) false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecDictListValueHelper =
            if needsDictRcDecDictListValueHelper then generateDictRefCountDecHelper dictRefCountDecDictListValueHelperLabel MemoryModel.NoReleasePlan None false (Some dictRefCountDecListValueHelperLabel) false false None enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecTupleStringListValueHelper =
            if needsDictRcDecTupleStringListValueHelper then generateDictRefCountDecHelper dictRefCountDecTupleStringListValueHelperLabel MemoryModel.NoReleasePlan None false None false false (Some (16, dictTupleStringListValueReleasePlan)) enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecTupleStringListDictValueHelper =
            if needsDictRcDecTupleStringListDictValueHelper then generateDictRefCountDecHelper dictRefCountDecTupleStringListDictValueHelperLabel MemoryModel.NoReleasePlan None false None false false (Some (24, dictTupleStringListDictValueReleasePlan)) enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecDynamicKeyTupleStringListDictValueHelper =
            if needsDictRcDecDynamicKeyTupleStringListDictValueHelper then generateDictRefCountDecHelper dictRefCountDecDynamicKeyTupleStringListDictValueHelperLabel (MemoryModel.DynamicBufferRelease MemoryModel.DynamicStringBuffer) None false None false false (Some (24, dictTupleStringListDictValueReleasePlan)) enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let dictDecSumStringValueHelper =
            if needsDictRcDecSumStringValueHelper then generateDictRefCountDecHelper dictRefCountDecSumStringValueHelperLabel MemoryModel.NoReleasePlan None false None false false (Some (16, dictSumStringValueReleasePlan)) enableLeakCheck recordRegistry sumShapeRegistry
            else []
        in
        let closureDecHelper =
            if emitClosureRcDecHelper then
                generateClosureRefCountDecHelper enableLeakCheck recordRegistry sumShapeRegistry closurePayloadSizes closureCaptureTypes
            else
                []
        in
        let closureIncHelper =
            if needsClosureRcIncHelper then generateClosureRefCountIncHelper closurePayloadSizes
            else []
        in
        let streamDecHelper =
            if needsStreamRcDecHelper then
                generateStreamRefCountDecHelper {
                    functionName = ( streamRefCountDecHelperLabel
                    );
                    stackSize = ( 0
                    );
                    usedCalleeSaved = ( []
                    );
                    enableLeakCheck = ( enableLeakCheck
                    );
                    recordRegistry = ( recordRegistry
                    );
                    sumShapeRegistry = ( sumShapeRegistry
                    );
                    functionNames = ( FunctionIdMap.empty
                    )
                }
            else
                []
        in
        allInstrs @ listIncHelper @ listDecHelpers @ dictIncHelper @ plannedDictDecHelpers @ dictDecHelper @ dictDecDynamicKeyHelper @ dictDecDynamicValueHelper @ dictDecDynamicKeyValueHelper @ dictDecDynamicKeyDictValueHelper @ dictDecDynamicKeyDictListValueHelper @ dictDecListValueHelper @ dictDecDictValueHelper @ dictDecDictListValueHelper @ dictDecTupleStringListValueHelper @ dictDecTupleStringListDictValueHelper @ dictDecDynamicKeyTupleStringListDictValueHelper @ dictDecSumStringValueHelper @ closureIncHelper @ closureDecHelper @ streamDecHelper @ recursiveNominalRcDecHelpers @ generateCliArgvHelper () @ generateCliEnvironmentPackedHelper enableLeakCheck @ generateCliDirectoryCurrentHelper enableLeakCheck @ generateCliSetEnvHelper enableLeakCheck @ generateCliUnsetEnvHelper enableLeakCheck @ generateCliDirectoryListHelper enableLeakCheck @ (if needsCliGetEnvHelper then generateCliGetEnvHelper enableLeakCheck else []) @ (if needsCliProcessLifecycleHelpers then generateLinuxCliSpawnProcessHelper () @ generateLinuxCliProcessLifecycleHelpers enableLeakCheck else []) @ (if needsCliRunProcessHelper then generateLinuxCliRunProcessHelper enableLeakCheck else []) @ (if needsCliExecuteHelper then generateLinuxCliExecuteHelper enableLeakCheck else []) @ genOomHandler () @ genRuntimeErrorHandler ())
