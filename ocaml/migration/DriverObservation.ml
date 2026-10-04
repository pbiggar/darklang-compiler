(* Complete public driver boundary observations without opening checked catalogs. *)
open Dark_compiler
module C=CheckedAST
module R=AST_to_ANF
module M=StringOrder.Map
module F=SpecializationIdentity.FunctionSet
let tuple=SemanticJson.tuple
let str=SemanticJson.string
let list f xs=`List (List.map f xs)
let typ=SemanticAST.semanticType
let map f values=`Assoc ["map",list (fun (key,value)->tuple [str key;f value]) (M.bindings values)]
let ids f values=SemanticJson.union "FunctionIdMap" "FunctionIdMap" [`Assoc ["map",list (fun (key,value)->tuple [`Assoc ["kind",`String "uint64";"value",`String (SemanticJson.unsigned64 (AST.functionIdValue key))];f value]) (FunctionIdMap.toList values)]]
let set values=`Assoc ["set",list ProductionMIR.functionId (F.elements values)]
let strings values=`Assoc ["set",list str (StringOrder.Set.elements values)]
let fields=list (fun (name,typValue)->tuple [str name;typ typValue])
let returns=ids (fun (name,typValue)->tuple [str name;typ typValue])
let typeReg=map (fun (value:TypeRegistries.recordTypeInfo)->SemanticJson.record "RecordTypeInfo" ["TypeParams",list str value.TypeRegistries.typeParams;"Fields",fields value.TypeRegistries.fields])
let variant (owner,params,tag,values)=tuple [str owner;list str params;SemanticJson.int32 tag;list typ values]
let variants=map variant
let metadata (value:C.semanticMetadata)=SemanticJson.record "SemanticMetadata" ["TypeNames",`Assoc ["map",list (fun (key,value)->tuple [SemanticJson.union "TypeId" "TypeId" [SemanticJson.int32 (AST.MigrationObservation.typeOrdinal key)];str value]) (C.TypeIdMap.bindings value.C.typeNames)]]
let ownership (value:OwnedIR.callSignature)=
 let parameter=function OwnedIR.UnmanagedCallParameter->"UnmanagedCallParameter"|OwnedIR.BorrowedCallParameter->"BorrowedCallParameter"|OwnedIR.ConsumedCallParameter->"ConsumedCallParameter"|OwnedIR.UniqueCallParameter->"UniqueCallParameter" in
 let output=match value.OwnedIR.result with OwnedIR.UnmanagedCallResult->SemanticJson.union "CallResultOwnership" "UnmanagedCallResult" []|OwnedIR.BorrowedCallResult n->SemanticJson.union "CallResultOwnership" "BorrowedCallResult" [SemanticJson.int32 n]|OwnedIR.ProducedCallResult->SemanticJson.union "CallResultOwnership" "ProducedCallResult" []|OwnedIR.UniqueProducedCallResult->SemanticJson.union "CallResultOwnership" "UniqueProducedCallResult" [] in
 SemanticJson.record "CallSignature" ["Parameters",list (fun value->SemanticJson.union "CallParameterOwnership" (parameter value) []) value.OwnedIR.parameters;"Result",output]
let scope (value:Destruction.functionScopeContract)=SemanticJson.record "FunctionScopeContract" ["LocalDestruction",SemanticJson.union "ScopeDestruction" (match value.Destruction.localDestruction with Destruction.InertScope->"InertScope"|Destruction.UnprovenScope->"UnprovenScope") [];"Calls",set value.Destruction.calls]
let sums (value:LoweringPrimitives.sumMetadata)=SemanticJson.record "SumMetadata" ["Names",strings value.LoweringPrimitives.names;"Cases",map (map (fun (value:LoweringPrimitives.sumCase)->SemanticJson.record "SumCase" ["TypeParams",list str value.LoweringPrimitives.typeParams;"Tag",SemanticJson.int32 value.LoweringPrimitives.tag;"Fields",list typ value.LoweringPrimitives.fields])) value.LoweringPrimitives.cases]
let symbols value=tuple [SemanticJson.int32 (C.bindingCursor value);`Assoc ["kind",`String "uint64";"value",`String (SemanticJson.unsigned64 (C.nextFunctionOrdinal value))];map ProductionMIR.functionId (C.functionIds value);ids str (C.functionNames value);metadata (C.semanticMetadata value)]
let program value=let symbolsValue,tops=C.viewProgram value in tuple [symbols symbolsValue;list (function
 |C.Expression expr->tuple [str "Expression";str (CheckedStructuralFormat.expr expr)]
 |C.FunctionDef func->tuple [str "FunctionDef";ProductionMIR.functionId func.C.id;str func.C.name;list str func.C.typeParams;list (fun (id,typValue)->tuple [str (HostStructuralFormat.format (CheckedStructuralFormat.value (C.Local id)));typ typValue]) (NonEmptyList.toList (C.functionParameterTypes func));typ (C.functionReturnType func);str (CheckedStructuralFormat.expr func.C.body);SemanticJson.option SemanticAST.observationTypedRecursiveMember (Option.map C.semanticRecursiveMember func.C.recursion)]
 |C.ValueDef value->tuple [str "ValueDef";str value.C.name;str (CheckedStructuralFormat.expr (C.Local value.C.id));typ (C.semanticType value.C.typ);str (CheckedStructuralFormat.expr value.C.body)]
 |C.TypeDef (id,definition)->tuple [str "TypeDef";SemanticJson.int32 (AST.MigrationObservation.typeOrdinal id);SemanticAST.typeDef (C.semanticTypeDef definition)]) tops]
let registries (value:R.registries)=SemanticJson.record "Registries" [
 "ScopeContracts",ids scope value.R.scopeContracts;"InertFunctionScopes",set value.R.inertFunctionScopes;
 "TypeReg",typeReg value.R.typeReg;"TypeNames",metadata value.R.typeNames;
 "RecordFieldsReg",map fields value.R.recordFieldsReg;"RecordTypeParamsReg",map (list str) value.R.recordTypeParamsReg;
 "VariantLookup",variants value.R.variantLookup;"SumMetadata",sums value.R.sumMetadata;
 "RcSumShapeReg",ProductionANF.memoryModel_rcSumShapeRegistry value.R.rcSumShapeReg;
 "FuncReg",returns value.R.funcReg;"FunctionIds",map ProductionMIR.functionId value.R.functionIds;
 "FunctionNames",ids str value.R.functionNames;"FuncParams",map fields value.R.funcParams;
 "ModuleRegistry",map SemanticAST.observationModuleFunc value.R.moduleRegistry;
 "RecursiveMembers",ids SemanticAST.observationLoweredRecursiveMember value.R.recursiveMembers]
let declaration (value:SourcePreparation.declarationConversion)=tuple [symbols value.SourcePreparation.symbols;list ProductionANF.aNF_functionDef value.SourcePreparation.functions;registries value.SourcePreparation.registries;returns value.SourcePreparation.localReturnTypes]
let conversion (value:R.conversionResult)=SemanticJson.record "ConversionResult" [
 "Program",ProductionANF.aNF_program value.R.program;"OwnershipContracts",ids ownership value.R.ownershipContracts;
 "RecursiveMembers",ids SemanticAST.observationLoweredRecursiveMember value.R.recursiveMembers;
 "TypeReg",typeReg value.R.typeReg;"RecordFieldsReg",map fields value.R.recordFieldsReg;
 "RecordTypeParamsReg",map (list str) value.R.recordTypeParamsReg;"VariantLookup",variants value.R.variantLookup;
 "RcSumShapeReg",ProductionANF.memoryModel_rcSumShapeRegistry value.R.rcSumShapeReg;
 "FuncReg",returns value.R.funcReg;"FuncParams",map fields value.R.funcParams;"ModuleRegistry",map SemanticAST.observationModuleFunc value.R.moduleRegistry]
let user (value:R.userOnlyResult)=SemanticJson.record "UserOnlyResult" [
 "Symbols",symbols value.R.symbols;"ScopeContracts",ids scope value.R.scopeContracts;
 "InertFunctionScopes",set value.R.inertFunctionScopes;"UserFunctions",list ProductionANF.aNF_functionDef value.R.userFunctions;
 "OwnershipContracts",ids ownership value.R.ownershipContracts;"NonInlineableFunctionNames",set value.R.nonInlineableFunctionNames;
 "MainExpr",ProductionANF.aNF_aExpr value.R.mainExpr;"TypeReg",typeReg value.R.typeReg;"TypeNames",metadata value.R.typeNames;
 "RecordFieldsReg",map fields value.R.recordFieldsReg;"RecordTypeParamsReg",map (list str) value.R.recordTypeParamsReg;
 "VariantLookup",variants value.R.variantLookup;"SumMetadata",sums value.R.sumMetadata;
 "LocalRecordFieldsReg",map fields value.R.localRecordFieldsReg;"LocalVariantLookup",variants value.R.localVariantLookup;
 "RcSumShapeReg",ProductionANF.memoryModel_rcSumShapeRegistry value.R.rcSumShapeReg;"FuncReg",returns value.R.funcReg;
 "FunctionIds",map ProductionMIR.functionId value.R.functionIds;"FunctionNames",ids str value.R.functionNames;
 "LocalReturnTypes",returns value.R.localReturnTypes;"FuncParams",map fields value.R.funcParams;
 "ModuleRegistry",map SemanticAST.observationModuleFunc value.R.moduleRegistry;
 "RecursiveMembers",ids SemanticAST.observationLoweredRecursiveMember value.R.recursiveMembers]
