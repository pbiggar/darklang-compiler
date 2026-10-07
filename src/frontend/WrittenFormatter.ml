(*
   WrittenFormatter.ml - Conservative formatting for validated interpreter syntax.
   A syntax fingerprint without source positions. Reparse checks use this
   instead of comparing WrittenTypes ranges, which change when text is formatted.
*)
(* WrittenFormatter.ml - Typed range-free fingerprints and guarded conservative formatting. *)
let union name fields = name ^ "(" ^ String.concat "," fields ^ ")"
let record fields = "{" ^ String.concat ";" fields ^ "}"
let tuple fields = "(" ^ String.concat "," fields ^ ")"
let quoted text = "\"" ^ text ^ "\""
let option encode = function None -> "null" | Some value -> union "Some" [encode value]
let rec list encode = function [] -> "Empty()" | head :: tail -> union "Cons" [encode head; list encode tail]
let unsigned64 value = if value < 0L then Z.to_string (Z.add (Z.of_int64 value) (Z.shift_left Z.one 64)) else Int64.to_string value
let rec expr (value : WrittenTypes.expr) = match value with
  | WrittenTypes.EUnit field0 -> union "EUnit" [(fun _ -> "") field0]
  | WrittenTypes.EBool (field0, field1) -> union "EBool" [(fun _ -> "") field0; string_of_bool field1]
  | WrittenTypes.EInt (field0, field1) -> union "EInt" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Z.to_string value ^ ""])]
  | WrittenTypes.EInt64 (field0, field1, field2) -> union "EInt64" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Int64.to_string value ^ "L"]); (fun _ -> "") field2]
  | WrittenTypes.EInt8 (field0, field1, field2) -> union "EInt8" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; string_of_int value ^ "y"]); (fun _ -> "") field2]
  | WrittenTypes.EUInt8 (field0, field1, field2) -> union "EUInt8" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; string_of_int value ^ "uy"]); (fun _ -> "") field2]
  | WrittenTypes.EInt16 (field0, field1, field2) -> union "EInt16" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; string_of_int value ^ "s"]); (fun _ -> "") field2]
  | WrittenTypes.EUInt16 (field0, field1, field2) -> union "EUInt16" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; string_of_int value ^ "us"]); (fun _ -> "") field2]
  | WrittenTypes.EInt32 (field0, field1, field2) -> union "EInt32" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Int32.to_string value ^ ""]); (fun _ -> "") field2]
  | WrittenTypes.EUInt32 (field0, field1, field2) -> union "EUInt32" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Int64.to_string value ^ "u"]); (fun _ -> "") field2]
  | WrittenTypes.EUInt64 (field0, field1, field2) -> union "EUInt64" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; unsigned64 value ^ "UL"]); (fun _ -> "") field2]
  | WrittenTypes.EInt128 (field0, field1, field2) -> union "EInt128" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Z.to_string value ^ ""]); (fun _ -> "") field2]
  | WrittenTypes.EUInt128 (field0, field1, field2) -> union "EUInt128" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Z.to_string value ^ ""]); (fun _ -> "") field2]
  | WrittenTypes.EFloat (field0, field1, field2, field3) -> union "EFloat" [(fun _ -> "") field0; string_of_bool field1; quoted field2; quoted field3]
  | WrittenTypes.EChar (field0, field1, field2, field3) -> union "EChar" [(fun _ -> "") field0; option (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; quoted part1])) field1; (fun _ -> "") field2; (fun _ -> "") field3]
  | WrittenTypes.EString (field0, field1, field2, field3, field4) -> union "EString" [(fun _ -> "") field0; option (fun item -> (fun _ -> "") item) field1; list (fun item -> stringSegment item) field2; (fun _ -> "") field3; (fun _ -> "") field4]
  | WrittenTypes.EVariable (field0, field1) -> union "EVariable" [(fun _ -> "") field0; quoted field1]
  | WrittenTypes.EFnName (field0, field1) -> union "EFnName" [(fun _ -> "") field0; qualifiedFnIdentifier field1]
  | WrittenTypes.EInfix (field0, field1, field2, field3) -> union "EInfix" [(fun _ -> "") field0; (let part0, part1 = field1 in tuple [(fun _ -> "") part0; infix part1]); expr field2; expr field3]
  | WrittenTypes.ELet (field0, field1, field2, field3, field4, field5) -> union "ELet" [(fun _ -> "") field0; letPattern field1; expr field2; expr field3; (fun _ -> "") field4; (fun _ -> "") field5]
  | WrittenTypes.EApply (field0, field1, field2, field3) -> union "EApply" [(fun _ -> "") field0; expr field1; list (fun item -> typeReference item) field2; list (fun item -> expr item) field3]
  | WrittenTypes.EList (field0, field1, field2, field3) -> union "EList" [(fun _ -> "") field0; list (fun item -> (let part0, part1 = item in tuple [expr part0; option (fun item -> (fun _ -> "") item) part1])) field1; (fun _ -> "") field2; (fun _ -> "") field3]
  | WrittenTypes.ETuple (field0, field1, field2, field3, field4, field5, field6) -> union "ETuple" [(fun _ -> "") field0; expr field1; (fun _ -> "") field2; expr field3; list (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; expr part1])) field4; (fun _ -> "") field5; (fun _ -> "") field6]
  | WrittenTypes.EIf (field0, field1, field2, field3, field4, field5, field6) -> union "EIf" [(fun _ -> "") field0; expr field1; expr field2; option (fun item -> expr item) field3; (fun _ -> "") field4; (fun _ -> "") field5; option (fun item -> (fun _ -> "") item) field6]
  | WrittenTypes.ERecordFieldAccess (field0, field1, field2, field3) -> union "ERecordFieldAccess" [(fun _ -> "") field0; expr field1; (let part0, part1 = field2 in tuple [(fun _ -> "") part0; quoted part1]); (fun _ -> "") field3]
  | WrittenTypes.ELambda (field0, field1, field2, field3, field4) -> union "ELambda" [(fun _ -> "") field0; list (fun item -> letPattern item) field1; expr field2; (fun _ -> "") field3; (fun _ -> "") field4]
  | WrittenTypes.ERecord (field0, field1, field2, field3, field4) -> union "ERecord" [(fun _ -> "") field0; qualifiedTypeIdentifier field1; list (fun item -> (let part0, part1, part2 = item in tuple [(fun _ -> "") part0; (let part0, part1 = part1 in tuple [(fun _ -> "") part0; quoted part1]); expr part2])) field2; (fun _ -> "") field3; (fun _ -> "") field4]
  | WrittenTypes.EDict (field0, field1, field2, field3, field4) -> union "EDict" [(fun _ -> "") field0; list (fun item -> (let part0, part1, part2, part3 = item in tuple [(fun _ -> "") part0; expr part1; (fun _ -> "") part2; expr part3])) field1; (fun _ -> "") field2; (fun _ -> "") field3; (fun _ -> "") field4]
  | WrittenTypes.ERecordUpdate (field0, field1, field2, field3, field4, field5) -> union "ERecordUpdate" [(fun _ -> "") field0; expr field1; list (fun item -> (let part0, part1, part2 = item in tuple [(let part0, part1 = part0 in tuple [(fun _ -> "") part0; quoted part1]); (fun _ -> "") part1; expr part2])) field2; (fun _ -> "") field3; (fun _ -> "") field4; (fun _ -> "") field5]
  | WrittenTypes.EEnum (field0, field1, field2, field3, field4) -> union "EEnum" [(fun _ -> "") field0; qualifiedTypeIdentifier field1; (let part0, part1 = field2 in tuple [(fun _ -> "") part0; quoted part1]); list (fun item -> expr item) field3; (fun _ -> "") field4]
  | WrittenTypes.EMatch (field0, field1, field2, field3, field4) -> union "EMatch" [(fun _ -> "") field0; expr field1; list (fun item -> matchCase item) field2; (fun _ -> "") field3; (fun _ -> "") field4]
  | WrittenTypes.EPipe (field0, field1, field2) -> union "EPipe" [(fun _ -> "") field0; expr field1; list (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; pipeExpr part1])) field2]
  | WrittenTypes.EStatement (field0, field1, field2) -> union "EStatement" [(fun _ -> "") field0; expr field1; expr field2]
  | WrittenTypes.EError field0 -> union "EError" [(fun _ -> "") field0]
and stringSegment (value : WrittenTypes.stringSegment) = match value with
  | WrittenTypes.StringText (field0, field1) -> union "StringText" [(fun _ -> "") field0; quoted field1]
  | WrittenTypes.StringInterpolation (field0, field1, field2, field3) -> union "StringInterpolation" [(fun _ -> "") field0; expr field1; (fun _ -> "") field2; (fun _ -> "") field3]
and matchCase (value : WrittenTypes.matchCase) = record [
  (fun _ -> "") value.WrittenTypes.barRange;
  matchPattern value.WrittenTypes.pat;
  (fun _ -> "") value.WrittenTypes.arrowRange;
  option (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; expr part1])) value.WrittenTypes.whenCondition;
  expr value.WrittenTypes.rhs]
and pipeExpr (value : WrittenTypes.pipeExpr) = match value with
  | WrittenTypes.EPipeInfix (field0, field1, field2) -> union "EPipeInfix" [(fun _ -> "") field0; (let part0, part1 = field1 in tuple [(fun _ -> "") part0; infix part1]); expr field2]
  | WrittenTypes.EPipeLambda (field0, field1, field2, field3, field4) -> union "EPipeLambda" [(fun _ -> "") field0; list (fun item -> letPattern item) field1; expr field2; (fun _ -> "") field3; (fun _ -> "") field4]
  | WrittenTypes.EPipeEnum (field0, field1, field2, field3, field4) -> union "EPipeEnum" [(fun _ -> "") field0; qualifiedTypeIdentifier field1; (let part0, part1 = field2 in tuple [(fun _ -> "") part0; quoted part1]); list (fun item -> expr item) field3; (fun _ -> "") field4]
  | WrittenTypes.EPipeFnCall (field0, field1, field2, field3) -> union "EPipeFnCall" [(fun _ -> "") field0; qualifiedFnIdentifier field1; list (fun item -> typeReference item) field2; list (fun item -> expr item) field3]
  | WrittenTypes.EPipeVariableOrFnCall (field0, field1) -> union "EPipeVariableOrFnCall" [(fun _ -> "") field0; quoted field1]
and qualifiedFnIdentifier (value : WrittenTypes.qualifiedFnIdentifier) = record [
  (fun _ -> "") value.WrittenTypes.range;
  list (fun item -> (let part0, part1 = item in tuple [identifier part0; (fun _ -> "") part1])) value.WrittenTypes.modules;
  identifier value.WrittenTypes.fn]
and qualifiedTypeIdentifier (value : WrittenTypes.qualifiedTypeIdentifier) = record [
  (fun _ -> "") value.WrittenTypes.range;
  list (fun item -> (let part0, part1 = item in tuple [identifier part0; (fun _ -> "") part1])) value.WrittenTypes.modules;
  identifier value.WrittenTypes.typ;
  list (fun item -> typeReference item) value.WrittenTypes.typeArgs]
and fnDecl (value : WrittenTypes.fnDecl) = record [
  (fun _ -> "") value.WrittenTypes.range;
  identifier value.WrittenTypes.name;
  list (fun item -> (let part0, part1 = item in tuple [quoted part0; (fun _ -> "") part1])) value.WrittenTypes.typeParams;
  list (fun item -> fnParam item) value.WrittenTypes.parameters;
  option (fun item -> list (fun item -> identifier item) item) value.WrittenTypes.effects;
  typeReference value.WrittenTypes.returnType;
  expr value.WrittenTypes.body;
  (fun _ -> "") value.WrittenTypes.keywordLet;
  (fun _ -> "") value.WrittenTypes.symbolColon;
  (fun _ -> "") value.WrittenTypes.symbolEquals;
  quoted value.WrittenTypes.description]
and valueDecl (value : WrittenTypes.valueDecl) = record [
  (fun _ -> "") value.WrittenTypes.range;
  identifier value.WrittenTypes.name;
  expr value.WrittenTypes.body;
  (fun _ -> "") value.WrittenTypes.keywordVal;
  (fun _ -> "") value.WrittenTypes.symbolEquals;
  quoted value.WrittenTypes.description]
and recordFieldSyntax (value : WrittenTypes.recordFieldSyntax) = record [
  (fun _ -> "") value.WrittenTypes.range;
  (let part0, part1 = value.WrittenTypes.name in tuple [(fun _ -> "") part0; quoted part1]);
  typeReference value.WrittenTypes.typ;
  quoted value.WrittenTypes.description;
  (fun _ -> "") value.WrittenTypes.symbolColon]
and enumFieldSyntax (value : WrittenTypes.enumFieldSyntax) = record [
  (fun _ -> "") value.WrittenTypes.range;
  typeReference value.WrittenTypes.typ;
  option (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; quoted part1])) value.WrittenTypes.label;
  option (fun item -> (fun _ -> "") item) value.WrittenTypes.symbolColon]
and enumCaseSyntax (value : WrittenTypes.enumCaseSyntax) = record [
  (fun _ -> "") value.WrittenTypes.range;
  (let part0, part1 = value.WrittenTypes.name in tuple [(fun _ -> "") part0; quoted part1]);
  list (fun item -> enumFieldSyntax item) value.WrittenTypes.fields;
  quoted value.WrittenTypes.description;
  option (fun item -> (fun _ -> "") item) value.WrittenTypes.keywordOf]
and typeDefinition (value : WrittenTypes.typeDefinition) = match value with
  | WrittenTypes.TDAlias field0 -> union "TDAlias" [typeReference field0]
  | WrittenTypes.TDRecord field0 -> union "TDRecord" [list (fun item -> (let part0, part1 = item in tuple [recordFieldSyntax part0; option (fun item -> (fun _ -> "") item) part1])) field0]
  | WrittenTypes.TDEnum field0 -> union "TDEnum" [list (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; enumCaseSyntax part1])) field0]
and typeDecl (value : WrittenTypes.typeDecl) = record [
  (fun _ -> "") value.WrittenTypes.range;
  identifier value.WrittenTypes.name;
  list (fun item -> (let part0, part1 = item in tuple [quoted part0; (fun _ -> "") part1])) value.WrittenTypes.typeParams;
  typeDefinition value.WrittenTypes.definition;
  (fun _ -> "") value.WrittenTypes.keywordType;
  (fun _ -> "") value.WrittenTypes.symbolEquals;
  quoted value.WrittenTypes.description]
and moduleDecl (value : WrittenTypes.moduleDecl) = record [
  (fun _ -> "") value.WrittenTypes.range;
  (let part0, part1 = value.WrittenTypes.name in tuple [(fun _ -> "") part0; quoted part1]);
  list (fun item -> declaration item) value.WrittenTypes.declarations;
  (fun _ -> "") value.WrittenTypes.keywordModule]
and testExpected (value : WrittenTypes.testExpected) = match value with
  | WrittenTypes.TEExpr field0 -> union "TEExpr" [expr field0]
  | WrittenTypes.TEError field0 -> union "TEError" [quoted field0]
  | WrittenTypes.TESqlError field0 -> union "TESqlError" [quoted field0]
and test (value : WrittenTypes.test) = record [
  (fun _ -> "") value.WrittenTypes.range;
  expr value.WrittenTypes.actual;
  testExpected value.WrittenTypes.expected]
and declaration (value : WrittenTypes.declaration) = match value with
  | WrittenTypes.DFunction field0 -> union "DFunction" [fnDecl field0]
  | WrittenTypes.DValue field0 -> union "DValue" [valueDecl field0]
  | WrittenTypes.DModule field0 -> union "DModule" [moduleDecl field0]
  | WrittenTypes.DType field0 -> union "DType" [typeDecl field0]
  | WrittenTypes.DExpr field0 -> union "DExpr" [expr field0]
  | WrittenTypes.DTypeDB field0 -> union "DTypeDB" [typeDecl field0]
  | WrittenTypes.DTest field0 -> union "DTest" [test field0]
and sourceFile (value : WrittenTypes.sourceFile) = record [
  (fun _ -> "") value.WrittenTypes.range;
  list (fun item -> declaration item) value.WrittenTypes.declarations;
  list (fun item -> expr item) value.WrittenTypes.exprsToEval]
and identifier (value : WrittenTypes.identifier) = record [
  (fun _ -> "") value.WrittenTypes.range;
  quoted value.WrittenTypes.name]
and typeReference (value : WrittenTypes.typeReference) = match value with
  | WrittenTypes.TUnit field0 -> union "TUnit" [(fun _ -> "") field0]
  | WrittenTypes.TBool field0 -> union "TBool" [(fun _ -> "") field0]
  | WrittenTypes.TInt field0 -> union "TInt" [(fun _ -> "") field0]
  | WrittenTypes.TInt8 field0 -> union "TInt8" [(fun _ -> "") field0]
  | WrittenTypes.TUInt8 field0 -> union "TUInt8" [(fun _ -> "") field0]
  | WrittenTypes.TInt16 field0 -> union "TInt16" [(fun _ -> "") field0]
  | WrittenTypes.TUInt16 field0 -> union "TUInt16" [(fun _ -> "") field0]
  | WrittenTypes.TInt32 field0 -> union "TInt32" [(fun _ -> "") field0]
  | WrittenTypes.TUInt32 field0 -> union "TUInt32" [(fun _ -> "") field0]
  | WrittenTypes.TInt64 field0 -> union "TInt64" [(fun _ -> "") field0]
  | WrittenTypes.TUInt64 field0 -> union "TUInt64" [(fun _ -> "") field0]
  | WrittenTypes.TInt128 field0 -> union "TInt128" [(fun _ -> "") field0]
  | WrittenTypes.TUInt128 field0 -> union "TUInt128" [(fun _ -> "") field0]
  | WrittenTypes.TFloat field0 -> union "TFloat" [(fun _ -> "") field0]
  | WrittenTypes.TChar field0 -> union "TChar" [(fun _ -> "") field0]
  | WrittenTypes.TString field0 -> union "TString" [(fun _ -> "") field0]
  | WrittenTypes.TDateTime field0 -> union "TDateTime" [(fun _ -> "") field0]
  | WrittenTypes.TUuid field0 -> union "TUuid" [(fun _ -> "") field0]
  | WrittenTypes.TBlob field0 -> union "TBlob" [(fun _ -> "") field0]
  | WrittenTypes.TList (field0, field1, field2, field3, field4) -> union "TList" [(fun _ -> "") field0; (fun _ -> "") field1; (fun _ -> "") field2; typeReference field3; (fun _ -> "") field4]
  | WrittenTypes.TDict (field0, field1, field2, field3, field4, field5, field6) -> union "TDict" [(fun _ -> "") field0; (fun _ -> "") field1; (fun _ -> "") field2; typeReference field3; (fun _ -> "") field4; typeReference field5; (fun _ -> "") field6]
  | WrittenTypes.TCustom field0 -> union "TCustom" [qualifiedTypeIdentifier field0]
  | WrittenTypes.TVariable (field0, field1, field2) -> union "TVariable" [(fun _ -> "") field0; (fun _ -> "") field1; (let part0, part1 = field2 in tuple [(fun _ -> "") part0; quoted part1])]
  | WrittenTypes.TTuple (field0, field1, field2, field3, field4, field5, field6) -> union "TTuple" [(fun _ -> "") field0; typeReference field1; (fun _ -> "") field2; typeReference field3; list (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; typeReference part1])) field4; (fun _ -> "") field5; (fun _ -> "") field6]
  | WrittenTypes.TFn (field0, field1, field2) -> union "TFn" [(fun _ -> "") field0; list (fun item -> (let part0, part1 = item in tuple [typeReference part0; (fun _ -> "") part1])) field1; typeReference field2]
and letPattern (value : WrittenTypes.letPattern) = match value with
  | WrittenTypes.LPUnit field0 -> union "LPUnit" [(fun _ -> "") field0]
  | WrittenTypes.LPVariable (field0, field1) -> union "LPVariable" [(fun _ -> "") field0; quoted field1]
  | WrittenTypes.LPWildcard field0 -> union "LPWildcard" [(fun _ -> "") field0]
  | WrittenTypes.LPTuple (field0, field1, field2, field3, field4, field5, field6) -> union "LPTuple" [(fun _ -> "") field0; letPattern field1; (fun _ -> "") field2; letPattern field3; list (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; letPattern part1])) field4; (fun _ -> "") field5; (fun _ -> "") field6]
and matchPattern (value : WrittenTypes.matchPattern) = match value with
  | WrittenTypes.MPVariable (field0, field1) -> union "MPVariable" [(fun _ -> "") field0; quoted field1]
  | WrittenTypes.MPInt (field0, field1) -> union "MPInt" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Z.to_string value ^ ""])]
  | WrittenTypes.MPInt8 (field0, field1, field2) -> union "MPInt8" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; string_of_int value ^ "y"]); (fun _ -> "") field2]
  | WrittenTypes.MPUInt8 (field0, field1, field2) -> union "MPUInt8" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; string_of_int value ^ "uy"]); (fun _ -> "") field2]
  | WrittenTypes.MPInt16 (field0, field1, field2) -> union "MPInt16" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; string_of_int value ^ "s"]); (fun _ -> "") field2]
  | WrittenTypes.MPUInt16 (field0, field1, field2) -> union "MPUInt16" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; string_of_int value ^ "us"]); (fun _ -> "") field2]
  | WrittenTypes.MPInt32 (field0, field1, field2) -> union "MPInt32" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Int32.to_string value ^ ""]); (fun _ -> "") field2]
  | WrittenTypes.MPUInt32 (field0, field1, field2) -> union "MPUInt32" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Int64.to_string value ^ "u"]); (fun _ -> "") field2]
  | WrittenTypes.MPInt64 (field0, field1, field2) -> union "MPInt64" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Int64.to_string value ^ "L"]); (fun _ -> "") field2]
  | WrittenTypes.MPUInt64 (field0, field1, field2) -> union "MPUInt64" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; unsigned64 value ^ "UL"]); (fun _ -> "") field2]
  | WrittenTypes.MPInt128 (field0, field1, field2) -> union "MPInt128" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Z.to_string value ^ ""]); (fun _ -> "") field2]
  | WrittenTypes.MPUInt128 (field0, field1, field2) -> union "MPUInt128" [(fun _ -> "") field0; (let _, value = field1 in tuple [""; Z.to_string value ^ ""]); (fun _ -> "") field2]
  | WrittenTypes.MPFloat (field0, field1, field2, field3) -> union "MPFloat" [(fun _ -> "") field0; string_of_bool field1; quoted field2; quoted field3]
  | WrittenTypes.MPBool (field0, field1) -> union "MPBool" [(fun _ -> "") field0; string_of_bool field1]
  | WrittenTypes.MPString (field0, field1, field2, field3) -> union "MPString" [(fun _ -> "") field0; option (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; quoted part1])) field1; (fun _ -> "") field2; (fun _ -> "") field3]
  | WrittenTypes.MPChar (field0, field1, field2, field3) -> union "MPChar" [(fun _ -> "") field0; option (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; quoted part1])) field1; (fun _ -> "") field2; (fun _ -> "") field3]
  | WrittenTypes.MPUnit field0 -> union "MPUnit" [(fun _ -> "") field0]
  | WrittenTypes.MPEnum (field0, field1, field2) -> union "MPEnum" [(fun _ -> "") field0; (let part0, part1 = field1 in tuple [(fun _ -> "") part0; quoted part1]); list (fun item -> matchPattern item) field2]
  | WrittenTypes.MPTuple (field0, field1, field2, field3, field4, field5, field6) -> union "MPTuple" [(fun _ -> "") field0; matchPattern field1; (fun _ -> "") field2; matchPattern field3; list (fun item -> (let part0, part1 = item in tuple [(fun _ -> "") part0; matchPattern part1])) field4; (fun _ -> "") field5; (fun _ -> "") field6]
  | WrittenTypes.MPList (field0, field1, field2, field3) -> union "MPList" [(fun _ -> "") field0; list (fun item -> (let part0, part1 = item in tuple [matchPattern part0; option (fun item -> (fun _ -> "") item) part1])) field1; (fun _ -> "") field2; (fun _ -> "") field3]
  | WrittenTypes.MPListCons (field0, field1, field2, field3) -> union "MPListCons" [(fun _ -> "") field0; matchPattern field1; matchPattern field2; (fun _ -> "") field3]
  | WrittenTypes.MPOr (field0, field1) -> union "MPOr" [(fun _ -> "") field0; list (fun item -> matchPattern item) field1]
  | WrittenTypes.MPError field0 -> union "MPError" [(fun _ -> "") field0]
and infix (value : WrittenTypes.infix) = match value with
  | WrittenTypes.InfixFnCall field0 -> union "InfixFnCall" [infixFnName field0]
  | WrittenTypes.BinOp field0 -> union "BinOp" [binaryOperation field0]
and infixFnName (value : WrittenTypes.infixFnName) = match value with
  | WrittenTypes.ArithmeticPlus -> union "ArithmeticPlus" []
  | WrittenTypes.ArithmeticMinus -> union "ArithmeticMinus" []
  | WrittenTypes.ArithmeticMultiply -> union "ArithmeticMultiply" []
  | WrittenTypes.ArithmeticDivide -> union "ArithmeticDivide" []
  | WrittenTypes.ArithmeticModulo -> union "ArithmeticModulo" []
  | WrittenTypes.ArithmeticPower -> union "ArithmeticPower" []
  | WrittenTypes.BitwiseAnd -> union "BitwiseAnd" []
  | WrittenTypes.BitwiseOr -> union "BitwiseOr" []
  | WrittenTypes.BitwiseXor -> union "BitwiseXor" []
  | WrittenTypes.ShiftLeft -> union "ShiftLeft" []
  | WrittenTypes.ShiftRight -> union "ShiftRight" []
  | WrittenTypes.ComparisonGreaterThan -> union "ComparisonGreaterThan" []
  | WrittenTypes.ComparisonGreaterThanOrEqual -> union "ComparisonGreaterThanOrEqual" []
  | WrittenTypes.ComparisonLessThan -> union "ComparisonLessThan" []
  | WrittenTypes.ComparisonLessThanOrEqual -> union "ComparisonLessThanOrEqual" []
  | WrittenTypes.ComparisonEquals -> union "ComparisonEquals" []
  | WrittenTypes.ComparisonNotEquals -> union "ComparisonNotEquals" []
  | WrittenTypes.StringConcat -> union "StringConcat" []
and binaryOperation (value : WrittenTypes.binaryOperation) = match value with
  | WrittenTypes.BinOpAnd -> union "BinOpAnd" []
  | WrittenTypes.BinOpOr -> union "BinOpOr" []
and fnParam (value : WrittenTypes.fnParam) = match value with
  | WrittenTypes.FPUnit field0 -> union "FPUnit" [(fun _ -> "") field0]
  | WrittenTypes.FPNormal (field0, field1, field2, field3, field4, field5, field6) -> union "FPNormal" [(fun _ -> "") field0; identifier field1; typeReference field2; (fun _ -> "") field3; (fun _ -> "") field4; (fun _ -> "") field5; quoted field6]
let syntaxKey = sourceFile
let stripAtomicParens source =
  let length = String.length source in
  let alpha char = char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z' || char = '_' in
  let digit char = char >= '0' && char <= '9' in
  let continue char = alpha char || digit char in
  let rec scan predicate index = if index < length && predicate source.[index] then scan predicate (index + 1) else index in
  let ending start =
    if start >= length then None
    else if alpha source.[start] then Some (scan continue (start + 1))
    else
      let first = if source.[start] = '-' then start + 1 else start in
      if first < length && digit source.[first] then
        let stop = scan digit (first + 1) in Some (if stop < length && source.[stop] = 'L' then stop + 1 else stop)
      else None in
  let buffer = Buffer.create length in
  let rec loop index = if index < length then
    if source.[index] = '(' then match ending (index + 1) with
      | Some stop when stop < length && source.[stop] = ')' -> Buffer.add_substring buffer source (index + 1) (stop - index - 1); loop (stop + 1)
      | _ -> Buffer.add_char buffer source.[index]; loop (index + 1)
    else begin Buffer.add_char buffer source.[index]; loop (index + 1) end in
  loop 0; Buffer.contents buffer
(*
   Preserve the interpreter's accepted layout while normalizing Unicode and
   redundant parentheses around atomic application arguments. A candidate is
   used only when the interpreter reparses it to the same range-free syntax tree.
*)
let format source parsed =
  let originalKey = syntaxKey parsed in
  let accept candidate fallback = match WrittenParsing.parse Validation.Script candidate with
    | Ok validated when syntaxKey (Validation.ValidatedSourceFile.toWrittenTypes validated) = originalKey -> candidate
    | Ok _ | Error _ -> fallback in
  let normalized = accept (Text.normalize source) source in
  accept (stripAtomicParens normalized) normalized
