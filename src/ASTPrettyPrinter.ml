(* ASTPrettyPrinter.fs - Pretty printer for canonical Dark syntax. *)
[@@@warning "-4"]
open AST
type literalEscapeContext=StringContent|InterpolatedStringText|CharContent
let escapeLiteralContent context input=
 let units=Text.scalars input in
 let output=Buffer.create (String.length input) in
 Array.iter (fun c->Buffer.add_string output (match c,context with
 |92,_->"\\\\"|34,(StringContent|InterpolatedStringText)->"\\\""|39,CharContent->"\\'"|10,_->"\\n"|13,_->"\\r"|9,_->"\\t"|0,_->"\\0"|123,InterpolatedStringText->"\\{"|125,InterpolatedStringText->"\\}"|_->Text.ofScalars [|c|])) units;
 Buffer.contents output
let formatFloatLiteral value=
 let raw=FloatFormat.roundTrip value in
 if String.exists (fun c -> c = 'N' || c = 'I') raw || String.contains raw '.' || String.contains raw 'E' || String.contains raw 'e' then raw else raw^".0"
let formatIdentifierSegment name=NameSyntax.formatIdentifier (NameSyntax.identifierFromText name)
let formatIdentifierPath name=match NameSyntax.tryParseLegacySpelling name with Some parsed->NameSyntax.formatQualifiedName parsed|None->formatIdentifierSegment name
let join delimiter f values=String.concat delimiter (List.map f values)
let wrap text="("^text^")"
let rec formatType=function
 |TInt8->"Int8"|TInt16->"Int16"|TInt32->"Int32"|TInt64->"Int64"|TInt128->"Int128"|TInt->"Int"|TUInt8->"UInt8"|TUInt16->"UInt16"|TUInt32->"UInt32"|TUInt64->"UInt64"|TUInt128->"UInt128"|TBool->"Bool"|TFloat64->"Float"|TString->"String"|TBlob->"Blob"|TChar->"Char"|TDateTime->"DateTime"|TUnit->"Unit"|TNever->"RuntimeError"|TInternalRawPtr->"RawPtr"
 |TVar name|TInferenceVar (name,_)->formatIdentifierSegment name
 |TList typ->"List<"^formatType typ^">"|TStream typ->"Stream<"^formatType typ^">"
 |TDict (key,value)->
  (* Explicit key and value arguments also parse inside nested generic calls. *)
  "Dict<"^formatType key^", "^formatType value^">"
 |TTuple types->wrap (join " * " (fun typ->match typ with TFunction _->wrap (formatType typ)|_->formatType typ) types)
 |TRecord (name,[])|TSum (name,[])->formatIdentifierPath name
 |TRecord (name,args)|TSum (name,args)->formatIdentifierPath name^"<"^join ", " formatType args^">"
 |TFunction (parameters,result)->String.concat " -> " (List.map (fun typ->match typ with TFunction _->wrap (formatType typ)|_->formatType typ) parameters@[formatType result])
let formatBinOp=function Add->"+"|Sub->"-"|Mul->"*"|Div->"/"|Mod->"%"|Pow->"^"|Shl->"<<"|Shr->">>"|BitAnd->"&"|BitOr->"|||"|BitXor->"^"|StringConcat->"++"|Eq->"=="|Neq->"!="|Lt->"<"|Gt->">"|Lte->"<="|Gte->">="|And->"&&"|Or->"||"
let formatUnaryOp=function Neg->"-"|Not->"!"|BitNot->"~~~"
let isComparisonOp=function Eq|Neq|Lt|Gt|Lte|Gte->true|Add|Sub|Mul|Div|Mod|Pow|Shl|Shr|BitAnd|BitOr|BitXor|StringConcat|And|Or->false
let binOpPrecedence=function Or->1|And->2|BitOr->3|BitXor->4|BitAnd->5|Eq|Neq|Lt|Gt|Lte|Gte->6|Shl|Shr->7|Add|Sub|StringConcat->8|Mul|Div|Mod->9|Pow->10
let shouldParenthesizeBinChild parent isLeft child=
 let parentPrec=binOpPrecedence parent and childPrec=binOpPrecedence child in
 if childPrec<parentPrec then true else if childPrec>parentPrec then false else if isComparisonOp parent then true else if parent=Pow then
 (* Exponentiation is right-associative. *)
 isLeft else
 (* Operators are left-associative: left child can omit equal-precedence
    parentheses, right child needs them to preserve tree shape. *)
 not isLeft
let rec isAtomicExpr=function
 |UnitLiteral|Int64Literal _|Int128Literal _|BigIntLiteral _|Int8Literal _|Int16Literal _|Int32Literal _|UInt8Literal _|UInt16Literal _|UInt32Literal _|UInt64Literal _|UInt128Literal _|BoolLiteral _|StringLiteral _|CharLiteral _|FloatLiteral _|InterpolatedString _|Var _|Apply _|IndirectApply _|TupleLiteral _|DictLiteral _|RecordLiteral _|ListLiteral _|Constructor (_,_,[])->true
 |TupleAccess (expr,_)|RecordAccess (expr,_)->isAtomicExpr expr|_->false
let parenthesizeIfNeeded expr text=if isAtomicExpr expr then text else wrap text
let parenthesizeTupleBaseIfNeeded expr text=match expr with TupleAccess _->wrap text|_->parenthesizeIfNeeded expr text
let isUnitLambdaParameter (parameter:lambdaParameter)=parameter.pattern=LPUnit
let isSyntheticUnitParamList parameters=match AST.NonEmptyList.toList parameters with [(name,TUnit)]->String.starts_with ~prefix:"$unit" name|_->false
let isUnitArgumentList args=match AST.NonEmptyList.toList args with [UnitLiteral]->true|_->false
let stringLiteral context text=escapeLiteralContent context text
let quoted text="\""^stringLiteral StringContent text^"\""
let character text="'"^stringLiteral CharContent text^"'"
let rec formatPattern=function
 |PUnit->"()"|PWildcard->"_"|PVar name->formatIdentifierSegment name
 |PConstructor (name,[])|PResolvedConstructor (_,name,_,[])->formatIdentifierPath name
 |PConstructor (name,[field])|PResolvedConstructor (_,name,_,[field])->let text=formatPattern field in formatIdentifierPath name^" "^(match field with PTuple _->wrap text|_->text)
 |PConstructor (name,fields)|PResolvedConstructor (_,name,_,fields)->formatIdentifierPath name^wrap (join ", " formatPattern fields)
 |POr alternatives->join " | " formatPattern (AST.NonEmptyList.toList alternatives)
 |PInt64 n->Int64.to_string n^"L"|PBigInt n->Z.to_string n^"I"|PInt128Literal n->Z.to_string n^"Q"|PInt8Literal n->string_of_int n^"y"|PInt16Literal n->string_of_int n^"s"|PInt32Literal n->Int32.to_string n^"l"|PUInt8Literal n->string_of_int n^"uy"|PUInt16Literal n->string_of_int n^"us"|PUInt32Literal n->Int64.to_string n^"ul"|PUInt64Literal n->Printf.sprintf "%LuUL" n|PUInt128Literal n->Z.to_string n^"Z"|PBool b->string_of_bool b|PString s->quoted s|PChar s->character s|PFloat f->formatFloatLiteral f
 |PTuple patterns->wrap (join ", " formatPattern patterns)|PList patterns->"["^join ", " formatPattern patterns^"]"
 |PListCons (head,tail)->
  let formatHead pattern=let formatted=formatPattern pattern in match pattern with
  (* Cons is right-associative. A cons used as a head therefore needs
     grouping or reparsing would flatten it into the outer chain. *)
  |PListCons _->wrap formatted
  (* Constructor payloads are whitespace-delimited and parse a
     complete pattern, so grouping keeps the outer cons outside the payload. *)
  |PConstructor (_, _::_)->wrap formatted|_->formatted in
  String.concat " :: " (List.map formatHead head@[formatPattern tail])
let rec formatLetPattern=function LPUnit->"()"|LPWildcard->"_"|LPVariable name->formatIdentifierSegment name|LPTuple (first,second,rest)->wrap (join ", " formatLetPattern (first::second::rest))
let rec formatExpr (expr:expr)=
 let isNegativeNumericLiteral=function Int64Literal n->n<0L|Int128Literal n|BigIntLiteral n->Z.sign n<0|Int8Literal n|Int16Literal n->n<0|Int32Literal n->n<0l|FloatLiteral f->
 (* Keep -0.0 wrapped as well; it is lexically ambiguous in application position. *)
 Int64.bits_of_float f<0L|_->false in
 let formatAppArg arg=let text=formatExpr arg in match arg with _ when isNegativeNumericLiteral arg->wrap text|Constructor (_,_,[])|TupleLiteral _|Apply _|IndirectApply _->wrap text|_->parenthesizeIfNeeded arg text in
 let rec formatAppArgs=function
 |[]->[]|[last]->[formatAppArg last]
 |current::(UnitLiteral as next)::rest->
  (* `f x ()` can be reparsed as applying unit to `x`.
     Parenthesize the preceding argument to preserve argument boundaries. *)
  wrap (formatAppArg current)::formatAppArgs (next::rest)
 |current::(TupleLiteral _ as next)::rest->
  (* `f g (a, b)` can be reparsed as applying `g` to tuple elements.
     Parenthesize the preceding argument to keep tuple as a separate argument. *)
  wrap (formatAppArg current)::formatAppArgs (next::rest)
 |current::rest->formatAppArg current::formatAppArgs rest in
 let annotated parameters=List.map (fun (parameter:lambdaParameter)->match parameter.pattern,parameter.sourceAnnotation with LPVariable name,Some typ->Some (wrap (formatIdentifierSegment name^": "^formatType typ))|_->None) (AST.NonEmptyList.toList parameters) in
 match expr with
 |BoundaryRender (_,value)->formatExpr value|RuntimeError message->"Builtin.testRuntimeError "^quoted message
 |UnitLiteral->"()"|Int64Literal n->Int64.to_string n^"L"|Int128Literal n->Z.to_string n^"Q"|BigIntLiteral n->Z.to_string n|Int8Literal n->string_of_int n^"y"|Int16Literal n->string_of_int n^"s"|Int32Literal n->Int32.to_string n^"l"|UInt8Literal n->string_of_int n^"uy"|UInt16Literal n->string_of_int n^"us"|UInt32Literal n->Int64.to_string n^"ul"|UInt64Literal n->Printf.sprintf "%LuUL" n|UInt128Literal n->Z.to_string n^"Z"|BoolLiteral b->string_of_bool b|StringLiteral s->quoted s|CharLiteral c->character c|FloatLiteral f->formatFloatLiteral f
 |InterpolatedString parts->"$\""^join "" (function StringText t->escapeLiteralContent InterpolatedStringText t|StringExpr e->"{"^formatExpr e^"}") parts^"\""
 |BinOp (op,left,right)->
  let formatChild isLeft child=let text=formatExpr child in match child with BinOp (childOp,_,_)->if shouldParenthesizeBinChild op isLeft childOp then wrap text else text|_->parenthesizeIfNeeded child text in
  let leftCanConsumeNegativeNumericArg=function Var name when String.contains name '.'->true|Apply _|IndirectApply _|Constructor (_,_,[])->true|_->false in
  let isNumericLiteralExpr=function Int64Literal _|Int128Literal _|BigIntLiteral _|Int8Literal _|Int16Literal _|Int32Literal _|UInt8Literal _|UInt16Literal _|UInt32Literal _|UInt64Literal _|UInt128Literal _|FloatLiteral _->true|_->false in
  let leftText=formatChild true left in let rightTextBase=formatChild false right in let rightText=match op with Sub when leftCanConsumeNegativeNumericArg left && isNumericLiteralExpr right->wrap rightTextBase|_->rightTextBase in leftText^" "^formatBinOp op^" "^rightText
 |UnaryOp (op,inner)->formatUnaryOp op^parenthesizeIfNeeded inner (formatExpr inner)
 |Let (LPVariable name,Lambda (parameters,Some returnType,functionBody),body)->
  let parametersText=annotated parameters in
  if List.for_all Option.is_some parametersText then "let "^formatIdentifierSegment name^" "^String.concat " " (List.filter_map Fun.id parametersText)^" : "^formatType returnType^" = "^formatExpr functionBody^" in "^formatExpr body else "let "^formatLetPattern (LPVariable name)^" = "^formatExpr (Lambda (parameters,Some returnType,functionBody))^" in "^formatExpr body
 |RecursiveLet (recursion,Lambda (parameters,Some returnType,functionBody),body) when recursiveBindingKind recursion=NamedLocalFunctionMember->
  let parametersText=annotated parameters in let name=recursiveBindingName recursion in
  if List.for_all Option.is_some parametersText then wrap ("let "^formatIdentifierSegment name^" "^String.concat " " (List.filter_map Fun.id parametersText)^" : "^formatType returnType^" = "^formatExpr functionBody^" in "^formatExpr body) else "let "^formatIdentifierSegment name^" = "^formatExpr (Lambda (parameters,Some returnType,functionBody))^" in "^formatExpr body
 |RecursiveLet (recursion,value,body)->"let "^formatIdentifierSegment (recursiveBindingName recursion)^" = "^formatExpr value^" in "^formatExpr body
 |Let (pattern,value,body)->"let "^formatLetPattern pattern^" = "^formatExpr value^" in "^formatExpr body
 |Var name->formatIdentifierPath name|If (cond,yes,no)->"if "^formatExpr cond^" then "^formatExpr yes^" else "^formatExpr no
 |Sequence (first,next)->wrap (formatExpr first^"; "^formatExpr next)
 |Apply (callee,typeArgs,args)->let head=parenthesizeIfNeeded callee (formatExpr callee)^(if typeArgs=[] then "" else "<"^join ", " formatType typeArgs^">") in head^" "^(if isUnitArgumentList args then "()" else String.concat " " (formatAppArgs (AST.NonEmptyList.toList args)))
 |TupleLiteral elements->wrap (join ", " formatExpr elements)
 |TupleAccess (base,index)->let text=formatExpr base in let text=match base with Apply _|IndirectApply _->
  (* Space application has no mandatory wrapping.
     Parenthesize before postfix access so `.0` binds to the call result. *)
  wrap text|_->parenthesizeTupleBaseIfNeeded base text in text^"."^string_of_int index
 |DictLiteral (_,_,entries)->"Dict { "^join "; " (fun (key,value)->formatExpr key^": "^formatExpr value) entries^" }"
 |RecordLiteral (reference,fields)->let fieldsText=join ", " (fun (reference,value)->formatIdentifierSegment reference.sourceFieldName^" = "^formatExpr value) fields in let args=match reference.typeArgs with []->""|args->"<"^join ", " formatType args^">" in formatIdentifierPath reference.sourceTypeName^args^" { "^fieldsText^" }"
 |RecordUpdate (base,updates)->"{ "^formatExpr base^" with "^join ", " (fun (reference,value)->formatIdentifierSegment reference.sourceFieldName^" = "^formatExpr value) updates^" }"
 |RecordAccess (base,field)->let text=formatExpr base in let text=match base with Apply _|IndirectApply _->
  (* Same ambiguity as tuple access: ensure `.field` applies to call result. *)
  wrap text|_->parenthesizeIfNeeded base text in text^"."^formatIdentifierSegment field.sourceFieldName
 |Constructor (reference,variant,fields)->let fullName=(match constructorReferenceTypeName reference with None->""|Some name->formatIdentifierPath name^".")^formatIdentifierSegment variant in (match fields with []->fullName|[field]->fullName^" "^formatAppArg field|_->fullName^wrap (join ", " formatExpr fields))
 |Match (scrutinee,cases)->
  let formatCaseBody body=let text=formatExpr body in match body with
  (* Without parens, nested match case bars get parsed as outer cases. *)
  |Match _|Let _->wrap text|_->text in
  let caseText=join " " (fun (case:matchCase)->let patterns=join " | " formatPattern (AST.NonEmptyList.toList case.patterns) in let guard=match case.guard with None->""|Some value->" when "^formatExpr value in "| "^patterns^guard^" -> "^formatCaseBody case.body) cases in "match "^formatExpr scrutinee^" with "^caseText
 |ListLiteral elements->"["^join ", " formatExpr elements^"]"
 |Lambda (parameters,_returnAnnotation,body)->let parameterList=AST.NonEmptyList.toList parameters in (match parameterList,body with
  |[{pattern=LPVariable name;inferredType=Some TBool;_}],BinOp (And,Var varName,right) when name="$pipe_arg" && varName="$pipe_arg"->"(&&) "^formatAppArg right
  |[{pattern=LPVariable name;inferredType=Some TBool;_}],BinOp (Or,Var varName,right) when name="$pipe_arg" && varName="$pipe_arg"->"(||) "^formatAppArg right
  |[parameter],_ when isUnitLambdaParameter parameter->"fun () -> "^formatExpr body
  |_->"fun "^join " " (fun (parameter:lambdaParameter)->formatLetPattern parameter.pattern) parameterList^" -> "^formatExpr body)
 |IndirectApply (callee,args)->(match callee,AST.NonEmptyList.toList args with
  (* Preserve the Apply-vs-Constructor distinction
     by printing constructor application in pipe form. *)
  |Constructor _,[single]->formatExpr single^" |> "^formatExpr callee
  |_->let text=parenthesizeIfNeeded callee (formatExpr callee) in text^" "^(if isUnitArgumentList args then "()" else String.concat " " (formatAppArgs (AST.NonEmptyList.toList args))))
 |Closure (name,captures)->"Closure("^formatIdentifierPath name^", ["^join ", " formatExpr captures^"])"
let formatFunctionDef (definition:functionDef)=
 let typeParams=if definition.typeParams=[] then "" else "<"^join ", " (fun name->"'"^name) definition.typeParams^">" in
 let parameters=if isSyntheticUnitParamList definition.params then "()" else join " " (fun (name,typ)->wrap (formatIdentifierSegment name^": "^formatType typ)) (AST.NonEmptyList.toList definition.params) in
 "let "^formatIdentifierSegment definition.name^typeParams^" "^parameters^" : "^formatType definition.returnType^" = "^formatExpr definition.body
let formatTypeDef definition=
 let formatTypeParams=function []->""|params->"<"^join ", " (fun name->"'"^name) params^">" in
 match definition with
 |RecordDef (name,parameters,fields)->"type "^formatIdentifierSegment name^formatTypeParams parameters^" = { "^join ", " (fun (name,typ)->formatIdentifierSegment name^": "^formatType typ) fields^" }"
 |SumTypeDef (name,parameters,variants)->"type "^formatIdentifierSegment name^formatTypeParams parameters^" = | "^join " | " (fun (variant:variant)->match variant.fields with []->formatIdentifierSegment variant.name|fields->formatIdentifierSegment variant.name^" of "^join " * " formatType fields) variants
 |TypeAlias (name,parameters,typ)->"type "^formatIdentifierSegment name^formatTypeParams parameters^" = "^formatType typ
let formatTopLevel=function FunctionDef definition->formatFunctionDef definition|TypeDef definition->formatTypeDef definition|ValueDef definition->"val "^formatIdentifierSegment (valueDefName definition)^" = "^formatExpr (valueDefBody definition)|Expression (_,expr)->formatExpr expr
let tryRestoreModuleDeclaration topLevel=
 let splitName name=Option.map (fun (moduleName,declarationName)->moduleName,NameSyntax.identifierText declarationName) (Option.bind (NameSyntax.tryParseLegacySpelling name) NameSyntax.trySplitLast) in
 match topLevel with
 |FunctionDef definition->Option.map (fun (moduleName,name)->moduleName,FunctionDef {definition with name}) (splitName definition.name)
 |ValueDef definition->Option.map (fun (moduleName,name)->let restored=match definition with UncheckedValueDef (_,body)->UncheckedValueDef (name,body)|CheckedValueDef (_,typ,body)->CheckedValueDef (name,typ,body) in moduleName,ValueDef restored) (splitName (valueDefName definition))
 |TypeDef (RecordDef (name,parameters,fields))->Option.map (fun (moduleName,name)->moduleName,TypeDef (RecordDef (name,parameters,fields))) (splitName name)
 |TypeDef (SumTypeDef (name,parameters,variants))->Option.map (fun (moduleName,name)->moduleName,TypeDef (SumTypeDef (name,parameters,variants))) (splitName name)
 |TypeDef (TypeAlias (name,parameters,typ))->Option.map (fun (moduleName,name)->moduleName,TypeDef (TypeAlias (name,parameters,typ))) (splitName name)
 |Expression _->None
let formatProgram (Program items)=
 let restored=List.map tryRestoreModuleDeclaration items in
 match restored with
 |Some (firstModule,_)::_ when List.for_all (function Some (moduleName,_)->moduleName=firstModule|None->false) restored->"module "^NameSyntax.formatQualifiedName firstModule^"\n"^join "\n" formatTopLevel (List.filter_map (Option.map snd) restored)
 |_->join "\n" formatTopLevel items
