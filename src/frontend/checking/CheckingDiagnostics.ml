(*
   CheckingDiagnostics.ml - Represent typing failures and render source-compatible diagnostics.
   `crash` is the public source-level bottom operation. The older builtin is
   retained solely for the test harness.
*)
(* CheckingDiagnostics.ml - Typing failures, source-compatible rendering, and inference identities. *)
(*
   Type errors
*)
type typeError =
  | TypeMismatch of AST.semanticType * AST.semanticType * string
  | IfBranchTypeMismatch of AST.semanticType * AST.semanticType
  | UndefinedVariable of string
  | UndefinedCallTarget of string
  | MissingTypeAnnotation of string
  | InvalidOperation of string * AST.semanticType list
  | IncompatibleEqualityOperands of AST.semanticType * AST.semanticType
  | IncompatibleOrderingOperands of AST.semanticType * AST.semanticType
  | PolymorphicRecursion of string
  | ResolutionFailure of NameResolution.resolutionError
  | GenericError of string

type aliasVisitState = AliasVisiting | AliasValidated

[@@@warning "-4"]

open! AST

let makePartialParams funcName types =
  let safeName = String.map (fun c -> if c = '.' then '_' else c) funcName in
  List.mapi (fun i t -> (Printf.sprintf "__partial_%s_%d" safeName i, t)) types

let toCallArgs = function
  | [] -> NonEmptyList.singleton UnitLiteral
  | args -> NonEmptyList.fromList args

let normalizeNullaryCallArgs expectedParamCount args =
  if expectedParamCount = 0 && args = [ UnitLiteral ] then [] else args

let toLambdaParams parameters =
  match
    List.map (fun (name, typ) -> inferredLambdaVariable name typ) parameters
    |> NonEmptyList.tryFromList
  with
  | Some nel -> nel
  | None ->
      Crash.crash
        "Type checker attempted to construct a lambda with zero parameters"

(*
   Pretty-print a type for error messages
   Type variable (for generics)
   Internal raw pointer type
*)
let rec typeToString = function
  | TInt8 -> "Int8"
  | TInt16 -> "Int16"
  | TInt32 -> "Int32"
  | TInt64 -> "Int64"
  | TInt128 -> "Int128"
  | TInt -> "Int"
  | TUInt8 -> "UInt8"
  | TUInt16 -> "UInt16"
  | TUInt32 -> "UInt32"
  | TUInt64 -> "UInt64"
  | TUInt128 -> "UInt128"
  | TBool -> "Bool"
  | TFloat64 -> "Float"
  | TString -> "String"
  | TBlob -> "Blob"
  | TChar -> "Char"
  | TDateTime -> "DateTime"
  | TUnit -> "Unit"
  | TNever -> "RuntimeError"
  | TInternalRawPtr -> "RawPtr"
  | TFunction (params, ret) ->
      "("
      ^ String.concat ", " (List.map typeToString params)
      ^ ") -> " ^ typeToString ret
  | TTuple elems -> "(" ^ String.concat ", " (List.map typeToString elems) ^ ")"
  | TRecord (name, []) | TSum (name, []) -> name
  | TRecord (name, args) | TSum (name, args) ->
      name ^ "<" ^ String.concat ", " (List.map typeToString args) ^ ">"
  | TList elem -> "List<" ^ typeToString elem ^ ">"
  | TStream elem -> "Stream<" ^ typeToString elem ^ ">"
  | TVar name | TInferenceVar (name, _) -> name
  | TDict (key, value) ->
      "Dict<" ^ typeToString key ^ ", " ^ typeToString value ^ ">"

(*
   Generated helpers and diagnostics both identify the complete semantic type.
*)
let rec typeToHelperIdentityString typ =
  let render = typeToHelperIdentityString in
  match typ with
  | TDict (key, value) -> "Dict<" ^ render key ^ ", " ^ render value ^ ">"
  | TList elem -> "List<" ^ render elem ^ ">"
  | TStream elem -> "Stream<" ^ render elem ^ ">"
  | TTuple elems -> "(" ^ String.concat ", " (List.map render elems) ^ ")"
  | TFunction (params, ret) ->
      "(" ^ String.concat ", " (List.map render params) ^ ") -> " ^ render ret
  | (TRecord (name, args) | TSum (name, args)) when args <> [] ->
      name ^ "<" ^ String.concat ", " (List.map render args) ^ ">"
  | _ -> typeToString typ

(*
   Pretty-print a type error
*)
let typeErrorToString = function
  | TypeMismatch (expected, actual, context) ->
      "Type mismatch in " ^ context ^ ": expected " ^ typeToString expected
      ^ ", got " ^ typeToString actual
  | IfBranchTypeMismatch (expected, actual) ->
      "Type mismatch: if branches must have same type: expected "
      ^ typeToString expected ^ ", got " ^ typeToString actual
  | UndefinedVariable name -> "Undefined variable: " ^ name
  | UndefinedCallTarget name -> "There is no variable named: " ^ name
  | MissingTypeAnnotation context -> "Missing type annotation: " ^ context
  | InvalidOperation (op, types) ->
      "Invalid operation '" ^ op ^ "' on types: "
      ^ String.concat ", " (List.map typeToString types)
  | IncompatibleEqualityOperands (left, right) ->
      "Cannot perform equality check on " ^ typeToString left ^ " and "
      ^ typeToString right
  | IncompatibleOrderingOperands (left, right) ->
      "Cannot perform numeric operation on " ^ typeToString left ^ " and "
      ^ typeToString right
  | PolymorphicRecursion name ->
      "Polymorphic recursion is not supported inside recursive group member: "
      ^ name
  | ResolutionFailure error -> NameResolution.errorToString error
  | GenericError msg -> msg

let withIndefiniteArticle s =
  if s = "" then s
  else
    let lower = Text.lowerInvariant s in
    (if List.mem lower.[0] [ 'a'; 'e'; 'i'; 'o'; 'u' ] then "an " else "a ") ^ s

let unsigned64 value =
  Z.to_string
    (if value < 0L then Z.add (Z.of_int64 value) (Z.shift_left Z.one 64)
     else Z.of_int64 value)

let describeIfConditionActual expr actualType =
  match expr with
  | UnitLiteral -> "Unit (())"
  | Int64Literal i -> "Int64 (" ^ Int64.to_string i ^ ")"
  | Int128Literal i -> "Int128 (" ^ Z.to_string i ^ ")"
  | Int8Literal i -> "Int8 (" ^ string_of_int i ^ ")"
  | Int16Literal i -> "Int16 (" ^ string_of_int i ^ ")"
  | Int32Literal i -> "Int32 (" ^ Int32.to_string i ^ ")"
  | UInt8Literal i -> "UInt8 (" ^ string_of_int i ^ ")"
  | UInt16Literal i -> "UInt16 (" ^ string_of_int i ^ ")"
  | UInt32Literal i -> "UInt32 (" ^ Int64.to_string i ^ ")"
  | UInt64Literal i -> "UInt64 (" ^ unsigned64 i ^ ")"
  | UInt128Literal i -> "UInt128 (" ^ Z.to_string i ^ ")"
  | StringLiteral s -> "String (\"" ^ s ^ "\")"
  | CharLiteral s -> "Char (\"" ^ s ^ "\")"
  | FloatLiteral f -> "Float (" ^ FloatFormat.roundTrip f ^ ")"
  | BoolLiteral b -> "Bool (" ^ string_of_bool b ^ ")"
  | _ -> typeToString actualType

let ifConditionTypeMismatchMessage expr actualType =
  "Encountered a condition that must be a Bool, but got "
  ^ withIndefiniteArticle (describeIfConditionActual expr actualType)

let formatFloatLiteralForPatternMismatch f =
  let formatted = FloatFormat.roundTrip f in
  if
    Text.contains formatted "."
    || Text.contains formatted "e"
    || Text.contains formatted "E"
  then formatted
  else formatted ^ ".0"

let describeInterpolationActual expr actualType =
  match expr with
  | FloatLiteral f -> "a Float (" ^ formatFloatLiteralForPatternMismatch f ^ ")"
  | Int64Literal i -> "an Int64 (" ^ Int64.to_string i ^ ")"
  | _ -> withIndefiniteArticle (typeToString actualType)

let interpolationTypeMismatchMessage expr actualType =
  let conversionModule =
    match actualType with
    | TInt8 -> Some "Int8"
    | TUInt8 -> Some "UInt8"
    | TInt16 -> Some "Int16"
    | TUInt16 -> Some "UInt16"
    | TInt32 -> Some "Int32"
    | TUInt32 -> Some "UInt32"
    | TInt64 -> Some "Int64"
    | TUInt64 -> Some "UInt64"
    | TInt128 -> Some "Int128"
    | TUInt128 -> Some "UInt128"
    | TInt -> Some "Int"
    | TFloat64 -> Some "Float"
    | TBool -> Some "Bool"
    | TChar -> Some "Char"
    | TDateTime -> Some "DateTime"
    | _ -> None
  in
  let hint =
    Option.fold ~none:""
      ~some:(fun name ->
        ". Try wrapping it with `Stdlib." ^ name ^ ".toString`.")
      conversionModule
  in
  "Expected String in string interpolation, got "
  ^ describeInterpolationActual expr actualType
  ^ " instead" ^ hint

(*
   Retain a let-bound literal in interpolation diagnostics. This substitution
   is deliberately limited to interpolation parts and respects lexical shadowing.
*)
let rec substituteInterpolationLiteral name literal expr =
  let recurse = substituteInterpolationLiteral name literal in
  match expr with
  | BoundaryRender (renderer, value) -> BoundaryRender (renderer, recurse value)
  | InterpolatedString parts ->
      InterpolatedString
        (List.map
           (function
             | StringText text -> StringText text
             | StringExpr (Var varName) when varName = name ->
                 StringExpr literal
             | StringExpr inner -> StringExpr (recurse inner))
           parts)
  | Let (pattern, value, body) ->
      Let
        ( pattern,
          recurse value,
          if List.mem name (letPatternBindings pattern) then body
          else recurse body )
  | RecursiveLet (recursion, value, body) ->
      if recursiveBindingName recursion = name then expr
      else RecursiveLet (recursion, recurse value, recurse body)
  | Lambda (parameters, _, _)
    when List.mem name
           (List.concat_map
              (fun parameter -> letPatternBindings parameter.pattern)
              (NonEmptyList.toList parameters)) ->
      expr
  | BinOp (op, left, right) -> BinOp (op, recurse left, recurse right)
  | UnaryOp (op, inner) -> UnaryOp (op, recurse inner)
  | If (condition, yes, no) -> If (recurse condition, recurse yes, recurse no)
  | Sequence (first, next) -> Sequence (recurse first, recurse next)
  | Apply (func, typeArgs, args) ->
      Apply (recurse func, typeArgs, NonEmptyList.map recurse args)
  | TupleLiteral elems -> TupleLiteral (List.map recurse elems)
  | TupleAccess (tuple, index) -> TupleAccess (recurse tuple, index)
  | DictLiteral (keyType, valueType, entries) ->
      DictLiteral
        ( keyType,
          valueType,
          List.map (fun (key, value) -> (recurse key, recurse value)) entries )
  | RecordLiteral (typeName, fields) ->
      RecordLiteral
        ( typeName,
          List.map (fun (field, value) -> (field, recurse value)) fields )
  | RecordUpdate (record, updates) ->
      RecordUpdate
        ( recurse record,
          List.map (fun (field, value) -> (field, recurse value)) updates )
  | RecordAccess (record, field) -> RecordAccess (recurse record, field)
  | Constructor (typeName, variant, fields) ->
      Constructor (typeName, variant, List.map recurse fields)
  | Match (scrutinee, cases) ->
      Match
        ( recurse scrutinee,
          List.map
            (fun case ->
              {
                case with
                guard = Option.map recurse case.guard;
                body = recurse case.body;
              })
            cases )
  | ListLiteral elems -> ListLiteral (List.map recurse elems)
  | Lambda (parameters, annotation, body) ->
      Lambda (parameters, annotation, recurse body)
  | IndirectApply (func, args) ->
      IndirectApply (recurse func, NonEmptyList.map recurse args)
  | Closure (name, captures) -> Closure (name, List.map recurse captures)
  | UnitLiteral | Int64Literal _ | Int128Literal _ | BigIntLiteral _
  | Int8Literal _ | Int16Literal _ | Int32Literal _ | UInt8Literal _
  | UInt16Literal _ | UInt32Literal _ | UInt64Literal _ | UInt128Literal _
  | BoolLiteral _ | StringLiteral _ | CharLiteral _ | FloatLiteral _ | Var _
  | RuntimeError _ ->
      expr

let isBuiltinUnwrapName name = name = "Builtin.unwrap"

(* Builtin.crash is the public source-level bottom operation. *)
let isSourceCrashName name = name = "Builtin.crash"

let isRuntimeFailureName name =
  isSourceCrashName name

let isBuiltinTestNanName name = name = "Builtin.testNan"
let isBuiltinTestInfinityName name = name = "Builtin.testInfinity"
let isBuiltinBlobEmptyName name = name = "Builtin.blobEmpty"

(*
   Whether a checked expression has semantic bottom type and therefore does
   not constrain a surrounding value-producing expression.
*)
let isNeverType typ = typ = TNever

let variantNameEndsWith suffix variantName =
  variantName = suffix || Filename.check_suffix variantName ("." ^ suffix)

let isKnownFailureConstructorExpr = function
  | Constructor (_, variant, []) when variantNameEndsWith "None" variant -> true
  | Constructor (_, variant, _ :: _) when variantNameEndsWith "Error" variant ->
      true
  | _ -> false

(*
   Detect runtime-failing unwrap expressions, including piped/desugared shapes:
   let x = Option.None in Builtin.unwrap(x)
*)
let rec isKnownUnwrapFailureExpr boundExprs expr =
  let rec argIsKnownFailure arg =
    isKnownFailureConstructorExpr arg
    ||
    match arg with
    | Var name ->
        Option.fold ~none:false ~some:argIsKnownFailure
          (StringOrder.Map.find_opt name boundExprs)
    | _ -> false
  in
  match expr with
  | Apply (Var funcName, [], { NonEmptyList.head = arg; tail = [] })
    when isBuiltinUnwrapName funcName ->
      argIsKnownFailure arg
  | Let (LPVariable name, value, body) ->
      isKnownUnwrapFailureExpr (StringOrder.Map.add name value boundExprs) body
  | Let (_, _, body) -> isKnownUnwrapFailureExpr boundExprs body
  | _ -> false

(*
   Detect known runtime-failing crash expressions, including let-bound forms.
*)
let rec isKnownCrashExpr boundExprs = function
  | Apply (Var funcName, [], { NonEmptyList.head = _; tail = [] })
    when isRuntimeFailureName funcName ->
      true
  | Let (LPVariable name, value, body) ->
      isKnownCrashExpr
        (StringOrder.Map.add name value boundExprs)
        body
  | Let (_, _, body) -> isKnownCrashExpr boundExprs body
  | Var name ->
      Option.fold ~none:false
        ~some:(isKnownCrashExpr boundExprs)
        (StringOrder.Map.find_opt name boundExprs)
  | _ -> false

(*
   Keep a stable diagnostic when the value is only known at runtime
   (for example a function parameter passed into Builtin.crash).
*)
let rec tryExtractStringLiteral boundExprs = function
  | StringLiteral s -> Some s
  | Var name -> (
      match StringOrder.Map.find_opt name boundExprs with
      | Some expr -> tryExtractStringLiteral boundExprs expr
      | None -> Some name)
  | Let (LPVariable name, value, body) ->
      tryExtractStringLiteral (StringOrder.Map.add name value boundExprs) body
  | Let (_, _, body) -> tryExtractStringLiteral boundExprs body
  | _ -> None

(*
   Extract the error message from a known Builtin.crash expression, if statically available.
*)
let rec tryExtractKnownCrashMessage boundExprs = function
  | Apply (Var funcName, [], { NonEmptyList.head = arg; tail = [] })
    when isRuntimeFailureName funcName ->
      tryExtractStringLiteral boundExprs arg
  | Let (LPVariable name, value, body) ->
      tryExtractKnownCrashMessage
        (StringOrder.Map.add name value boundExprs)
        body
  | Let (_, _, body) -> tryExtractKnownCrashMessage boundExprs body
  | Var name ->
      Option.bind
        (StringOrder.Map.find_opt name boundExprs)
        (tryExtractKnownCrashMessage boundExprs)
  | _ -> None

let rec tryFormatLiteralValue = function
  | UnitLiteral -> Some "()"
  | Int64Literal i -> Some (Int64.to_string i)
  | Int128Literal i | UInt128Literal i | BigIntLiteral i -> Some (Z.to_string i)
  | Int8Literal i | Int16Literal i | UInt8Literal i | UInt16Literal i ->
      Some (string_of_int i)
  | Int32Literal i -> Some (Int32.to_string i)
  | UInt32Literal i -> Some (Int64.to_string i)
  | UInt64Literal i -> Some (unsigned64 i)
  | BoolLiteral b -> Some (string_of_bool b)
  | StringLiteral s -> Some ("\"" ^ s ^ "\"")
  | CharLiteral c -> Some ("'" ^ c ^ "'")
  | FloatLiteral f -> Some (FloatFormat.roundTrip f)
  | TupleLiteral elems ->
      List.fold_left
        (fun acc elem ->
          match (acc, tryFormatLiteralValue elem) with
          | Some texts, Some text -> Some (texts @ [ text ])
          | _ -> None)
        (Some []) elems
      |> Option.map (fun items -> "(" ^ String.concat ", " items ^ ")")
  | _ -> None

let rec formatDeconstructionPattern = function
  | PVar _ -> "[variable]"
  | PWildcard -> "_"
  | PUnit -> "()"
  | PTuple patterns ->
      "("
      ^ String.concat ", " (List.map formatDeconstructionPattern patterns)
      ^ ")"
  | _ -> "[pattern]"

let rec formatLetDeconstructionPattern = function
  | LPVariable _ -> "[variable]"
  | LPWildcard -> "_"
  | LPUnit -> "()"
  | LPTuple (first, second, rest) ->
      "("
      ^ String.concat ", "
          (List.map formatLetDeconstructionPattern (first :: second :: rest))
      ^ ")"

let rec inferredLetPatternType path = function
  | LPUnit -> TUnit
  | LPVariable name -> TVar ("binding_" ^ path ^ "_" ^ name)
  | LPWildcard -> TVar ("binding_" ^ path ^ "_wildcard")
  | LPTuple (first, second, rest) ->
      TTuple
        (List.mapi
           (fun index inner ->
             inferredLetPatternType (Printf.sprintf "%s_%d" path index) inner)
           (first :: second :: rest))

(*
   Check the entire let pattern shape before returning any bindings.
*)
let rec bindLetPatternTypes pattern valueType =
  match (pattern, valueType) with
  | LPVariable name, typ -> Some [ (name, typ) ]
  | LPWildcard, _ -> Some []
  | LPUnit, (TUnit | TVar _ | TInferenceVar _) -> Some []
  | LPTuple _, (TVar _ | TInferenceVar _) ->
      bindLetPatternTypes pattern (inferredLetPatternType "tuple" pattern)
  | LPTuple (first, second, rest), TTuple types ->
      let patterns = first :: second :: rest in
      if List.length patterns <> List.length types then None
      else
        List.fold_left2
          (fun bindings inner typ ->
            match (bindings, bindLetPatternTypes inner typ) with
            | Some accumulated, Some xs -> Some (accumulated @ xs)
            | _ -> None)
          (Some []) patterns types
  | LPUnit, _ | LPTuple _, _ -> None

let formatListLiteralForNoMatch = function
  | [] -> "[]"
  | elems ->
      "[  "
      ^ String.concat ", "
          (List.map
             (fun elem ->
               Option.value (tryFormatLiteralValue elem) ~default:"<unknown>")
             elems)
      ^ "]"

(*
   For singleton list mismatches, report the mismatched element value.
*)
let rec formatPatternMismatchValue = function
  | ListLiteral (first :: second :: _) ->
      Some
        ("[  "
        ^ Option.value (formatPatternMismatchValue first) ~default:"<unknown>"
        ^ ", "
        ^ Option.value (formatPatternMismatchValue second) ~default:"<unknown>"
        ^ ", ...")
  | ListLiteral [ single ] -> formatPatternMismatchValue single
  | ListLiteral [] -> Some "[]"
  | FloatLiteral f -> Some (formatFloatLiteralForPatternMismatch f)
  | TupleLiteral elems ->
      Some
        ("("
        ^ String.concat ", "
            (List.map
               (fun elem ->
                 Option.value
                   (formatPatternMismatchValue elem)
                   ~default:"<unknown>")
               elems)
        ^ ")")
  | expr -> tryFormatLiteralValue expr

let rec narrowPatternMismatchExprByType actualType expr =
  match (actualType, expr) with
  | TList _, _ | TTuple _, _ -> expr
  | _, ListLiteral (first :: _) | _, TupleLiteral (first :: _) ->
      narrowPatternMismatchExprByType actualType first
  | _, _ -> expr

let formatPatternMismatchError scrutinee actualType expectedType override =
  let valueText =
    Option.value
      (formatPatternMismatchValue
         (narrowPatternMismatchExprByType actualType scrutinee))
      ~default:"<unknown>"
  in
  let expectedText =
    withIndefiniteArticle
      (Option.value override ~default:(typeToString expectedType))
  in
  "Cannot match " ^ typeToString actualType ^ " value " ^ valueText ^ " with "
  ^ expectedText ^ " pattern"

let formatLegacyParamTypeError functionName paramIndex paramName expectedType
    actualType actualExpr =
  let ordinal =
    match paramIndex with
    | 1 -> "1st"
    | 2 -> "2nd"
    | 3 -> "3rd"
    | _ -> string_of_int paramIndex ^ "th"
  in
  let actualValue =
    Option.value
      (tryFormatLiteralValue actualExpr)
      ~default:(typeToString actualType)
  in
  functionName ^ "'s " ^ ordinal ^ " parameter `" ^ paramName ^ "` expects "
  ^ typeToString expectedType ^ ", but got " ^ typeToString actualType ^ " ("
  ^ actualValue ^ ")"

(*
   Each invocation gets a fresh, structurally separate inference identity.
   The internal key cannot be a source type-variable spelling, so no caller
   environment scan is necessary.
*)
let inferenceVarForKey key =
  if String.starts_with ~prefix:"#infer:" key then
    let last = String.rindex key ':' in
    if last < 7 then Crash.crash "Malformed inference-variable identity"
    else TInferenceVar (String.sub key 7 (last - 7), key)
  else TVar key

let inferenceIdentity = Atomic.make 0

let freshenTypeParams scopeName typeParams =
  let freshParams =
    List.mapi
      (fun index base ->
        let displayName =
          base ^ "$"
          ^ Option.fold ~none:"" ~some:(fun scope -> scope ^ "$") scopeName
          ^ string_of_int index
        in
        "#infer:" ^ displayName ^ ":"
        ^ string_of_int (Atomic.fetch_and_add inferenceIdentity 1))
      typeParams
  in
  ( freshParams,
    List.fold_left2
      (fun subst name fresh -> StringOrder.Map.add name fresh subst)
      StringOrder.Map.empty typeParams freshParams )

(*
   Apply type variable renaming to a type
*)
let rec applyTypeVarRenaming subst typ =
  let recurse = applyTypeVarRenaming subst in
  match typ with
  | TVar name ->
      Option.fold ~none:typ ~some:inferenceVarForKey
        (StringOrder.Map.find_opt name subst)
  | TInferenceVar _ -> typ
  | TList elem -> TList (recurse elem)
  | TStream elem -> TStream (recurse elem)
  | TDict (key, value) -> TDict (recurse key, recurse value)
  | TFunction (params, ret) -> TFunction (List.map recurse params, recurse ret)
  | TTuple elems -> TTuple (List.map recurse elems)
  | TSum (name, args) -> TSum (name, List.map recurse args)
  | TRecord (name, args) -> TRecord (name, List.map recurse args)
  | TInt8 | TInt16 | TInt32 | TInt64 | TInt128 | TInt | TUInt8 | TUInt16
  | TUInt32 | TUInt64 | TUInt128 | TBool | TFloat64 | TString | TBlob | TChar
  | TDateTime | TUnit | TNever | TInternalRawPtr ->
      typ
