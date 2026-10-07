(*
   Validates a parsed WrittenTypes tree before it is lowered to ProgramTypes.
   Parsing is shared by scripts, packages, and tests, and may produce recovery
   nodes after syntax errors. This module applies the rules for each file mode
   and checks structural invariants, such as unique binders and compatible
   or-pattern bindings, that lowering relies on. It does not resolve names,
   check types, or evaluate expressions.
*)
(* Validation.ml - Preserve ordered pre-lowering structural and purpose diagnostics. *)
module WT = WrittenTypes
open WrittenTypes

type mode = Script | Package | Test

type issueCode =
  | DuplicateBinder
  | OrBindingMismatch
  | EmptyOrPattern
  | RecoveryHole
  | EmptyLambda
  | EmptyMatch
  | AnonymousRecord
  | EmptyRecordUpdate
  | PackageExpression
  | TestAssertion
  | DBMode
  | DBShape
  | TestMode

module IssueCode = struct
  let toString = function
    | DuplicateBinder -> "VALIDATION-DUPLICATE-BINDER"
    | OrBindingMismatch -> "VALIDATION-OR-BINDINGS"
    | EmptyOrPattern -> "VALIDATION-OR-PATTERN"
    | RecoveryHole -> "VALIDATION-RECOVERY-HOLE"
    | EmptyLambda -> "VALIDATION-LAMBDA"
    | EmptyMatch -> "VALIDATION-MATCH"
    | AnonymousRecord -> "VALIDATION-ANONYMOUS-RECORD"
    | EmptyRecordUpdate -> "VALIDATION-RECORD-UPDATE"
    | PackageExpression -> "VALIDATION-PACKAGE-EXPR"
    | TestAssertion -> "VALIDATION-TEST-ASSERTION"
    | DBMode -> "VALIDATION-DB-MODE"
    | DBShape -> "VALIDATION-DB-SHAPE"
    | TestMode -> "VALIDATION-TEST-MODE"
end

type issue = {
  range : WT.range;
  code : issueCode;
  message : string;
  related : (WT.range * string) list;
  hint : string option;
}

(*
   A source file that passed both structural and file-purpose validation.
   The case is private so production parsing paths cannot create one without
   calling `validate`.
*)
type validatedSourceFile = ValidatedSourceFile of mode * WT.sourceFile

module ValidatedSourceFile = struct
  let mode (ValidatedSourceFile (mode, _)) = mode
  let toWrittenTypes (ValidatedSourceFile (_, sourceFile)) = sourceFile
end

let detailedIssue range code message related hint =
  { range; code; message; related; hint }

let issue range code message = detailedIssue range code message [] None
let ignored name = name = "" || String.starts_with ~prefix:"_" name

let rec letBindings = function
  | WT.LPVariable (range, name) -> [ (name, range) ]
  | WT.LPUnit _ | WT.LPWildcard _ -> []
  | WT.LPTuple (_, first, _, second, rest, _, _) ->
      letBindings first @ letBindings second
      @ List.concat_map (fun (_, pattern) -> letBindings pattern) rest

(*
   A valid or-pattern has one logical binding set. Use the first alternative
   as its representative when checking an enclosing tuple/list pattern.
*)
let rec matchBindings = function
  | WT.MPVariable (range, name) -> [ (name, range) ]
  | WT.MPTuple (_, first, _, second, rest, _, _) ->
      matchBindings first @ matchBindings second
      @ List.concat_map (fun (_, pattern) -> matchBindings pattern) rest
  | WT.MPList (_, contents, _, _) ->
      List.concat_map (fun (pattern, _) -> matchBindings pattern) contents
  | WT.MPListCons (_, head, tail, _) -> matchBindings head @ matchBindings tail
  | WT.MPEnum (_, _, fields) -> List.concat_map matchBindings fields
  | WT.MPOr (_, first :: _) -> matchBindings first
  | WT.MPOr (_, [])
  | WT.MPUnit _ | WT.MPBool _ | WT.MPInt _ | WT.MPInt64 _ | WT.MPInt8 _
  | WT.MPUInt8 _ | WT.MPInt16 _ | WT.MPUInt16 _ | WT.MPInt32 _ | WT.MPUInt32 _
  | WT.MPUInt64 _ | WT.MPInt128 _ | WT.MPUInt128 _ | WT.MPFloat _ | WT.MPChar _
  | WT.MPString _ | WT.MPError _ ->
      []

module Names = Set.Make (String)

let usableNames pattern =
  matchBindings pattern |> List.map fst
  |> List.filter (fun name -> not (ignored name))
  |> Names.of_list

let duplicateIssues bindings =
  let bindings = List.filter (fun (name, _) -> not (ignored name)) bindings in
  let groups =
    List.fold_left
      (fun groups ((name, _) as binding) ->
        let previous = Option.value ~default:[] (List.assoc_opt name groups) in
        (name, binding :: previous) :: List.remove_assoc name groups)
      [] bindings
  in
  groups |> Prelude.Map.values
  |> List.concat_map (fun reversed ->
      match List.rev reversed with
      | (name, firstRange) :: (_ :: _ as duplicates) ->
          List.map
            (fun (_, range) ->
              detailedIssue range DuplicateBinder
                ("Duplicate binding '" ^ name ^ "' in the same pattern")
                [ (firstRange, "'" ^ name ^ "' was first bound here") ]
                (Some "use a different name or '_' for a value you do not need"))
            duplicates
      | [] | [ _ ] -> [])

let rec duplicatePatternIssues pattern =
  match pattern with
  | WT.MPOr (_, alternatives) ->
      List.concat_map duplicatePatternIssues alternatives
  | other -> duplicateIssues (matchBindings other)
[@@warning "-4"]

let rec structuralPatternIssues = function
  | WT.MPOr (range, []) ->
      [
        issue range EmptyOrPattern
          "An or-pattern must have at least one alternative";
      ]
  | WT.MPOr (_, (first :: rest as alternatives)) ->
      let nested = List.concat_map structuralPatternIssues alternatives in
      let firstNames = usableNames first in
      let unequal =
        List.filter_map
          (fun alternative ->
            if Names.equal (usableNames alternative) firstNames then None
            else
              Some
                (detailedIssue (WT.mpRange alternative) OrBindingMismatch
                   "Every branch of an or-pattern must bind the same names"
                   [
                     (WT.mpRange first, "the first branch binds a different set");
                   ]
                   None))
          rest
      in
      nested @ unequal
  | WT.MPError range ->
      [ issue range RecoveryHole "A recovered pattern cannot be lowered" ]
  | WT.MPList (_, contents, _, _) ->
      List.concat_map
        (fun (pattern, _) -> structuralPatternIssues pattern)
        contents
  | WT.MPListCons (_, head, tail, _) ->
      structuralPatternIssues head @ structuralPatternIssues tail
  | WT.MPTuple (_, first, _, second, rest, _, _) ->
      structuralPatternIssues first
      @ structuralPatternIssues second
      @ List.concat_map
          (fun (_, pattern) -> structuralPatternIssues pattern)
          rest
  | WT.MPEnum (_, _, fields) -> List.concat_map structuralPatternIssues fields
  | WT.MPUnit _ | WT.MPVariable _ | WT.MPBool _ | WT.MPInt _ | WT.MPInt64 _
  | WT.MPInt8 _ | WT.MPUInt8 _ | WT.MPInt16 _ | WT.MPUInt16 _ | WT.MPInt32 _
  | WT.MPUInt32 _ | WT.MPUInt64 _ | WT.MPInt128 _ | WT.MPUInt128 _
  | WT.MPFloat _ | WT.MPChar _ | WT.MPString _ ->
      []

let patternIssues pattern =
  duplicatePatternIssues pattern @ structuralPatternIssues pattern

let rec exprIssues expression =
  let recurse = exprIssues in
  let lambdaRequired range patterns =
    if patterns = [] then
      [ issue range EmptyLambda "A lambda must have at least one parameter" ]
    else []
  in
  match expression with
  | WT.EError range ->
      [ issue range RecoveryHole "A recovered expression cannot be lowered" ]
  | WT.ELambda (range, patterns, body, _, _) ->
      lambdaRequired range patterns
      @ duplicateIssues (List.concat_map letBindings patterns)
      @ recurse body
  | WT.ELet (_, pattern, value, body, _, _) ->
      duplicateIssues (letBindings pattern) @ recurse value @ recurse body
  | WT.EMatch (range, value, cases, _, _) ->
      (if cases = [] then
         [ issue range EmptyMatch "A match must have at least one case" ]
       else [])
      @ recurse value
      @ List.concat_map
          (fun (case : WT.matchCase) ->
            patternIssues case.pat
            @ Option.fold ~none:[]
                ~some:(fun (_, value) -> recurse value)
                case.whenCondition
            @ recurse case.rhs)
          cases
  | WT.ERecord (range, typeName, fields, _, _) ->
      (if typeName.typ.name = "" then
         [ issue range AnonymousRecord "Anonymous records are not supported" ]
       else [])
      @ List.concat_map (fun (_, _, value) -> recurse value) fields
  | WT.ERecordUpdate (range, record, updates, _, _, _) ->
      (if updates = [] then
         [
           issue range EmptyRecordUpdate
             "A record update must contain at least one 'field = value'";
         ]
       else [])
      @ recurse record
      @ List.concat_map (fun (_, _, value) -> recurse value) updates
  | WT.EString (_, _, segments, _, _) ->
      List.concat_map
        (function
          | WT.StringText _ -> []
          | WT.StringInterpolation (_, value, _, _) -> recurse value)
        segments
  | WT.EInfix (_, _, left, right) | WT.EStatement (_, left, right) ->
      recurse left @ recurse right
  | WT.EApply (_, callee, _, args) ->
      recurse callee @ List.concat_map recurse args
  | WT.EList (_, contents, _, _) ->
      List.concat_map (fun (value, _) -> recurse value) contents
  | WT.ETuple (_, first, _, second, rest, _, _) ->
      recurse first @ recurse second
      @ List.concat_map (fun (_, value) -> recurse value) rest
  | WT.EIf (_, condition, thenExpr, elseExpr, _, _, _) ->
      recurse condition @ recurse thenExpr
      @ Option.fold ~none:[] ~some:recurse elseExpr
  | WT.ERecordFieldAccess (_, record, _, _) -> recurse record
  | WT.EDict (_, entries, _, _, _) ->
      List.concat_map
        (fun (_, key, _, value) -> recurse key @ recurse value)
        entries
  | WT.EEnum (_, _, _, fields, _) -> List.concat_map recurse fields
  | WT.EPipe (_, first, parts) ->
      let pipeIssues = function
        | WT.EPipeLambda (range, patterns, body, _, _) ->
            lambdaRequired range patterns
            @ duplicateIssues (List.concat_map letBindings patterns)
            @ recurse body
        | WT.EPipeInfix (_, _, value) -> recurse value
        | WT.EPipeFnCall (_, _, _, args) -> List.concat_map recurse args
        | WT.EPipeEnum (_, _, _, fields, _) -> List.concat_map recurse fields
        | WT.EPipeVariableOrFnCall _ -> []
      in
      recurse first @ List.concat_map (fun (_, part) -> pipeIssues part) parts
  | WT.EUnit _ | WT.EBool _ | WT.EInt _ | WT.EInt64 _ | WT.EInt8 _ | WT.EUInt8 _
  | WT.EInt16 _ | WT.EUInt16 _ | WT.EInt32 _ | WT.EUInt32 _ | WT.EUInt64 _
  | WT.EInt128 _ | WT.EUInt128 _ | WT.EFloat _ | WT.EChar _ | WT.EVariable _
  | WT.EFnName _ ->
      []

let rec declarationStructureIssues = function
  | WT.DFunction fn ->
      duplicateIssues
        (List.filter_map
           (function
             | WT.FPNormal (_, name, _, _, _, _, _) ->
                 Some (name.name, name.range)
             | WT.FPUnit _ -> None)
           fn.parameters)
      @ exprIssues fn.body
  | WT.DValue value -> exprIssues value.body
  | WT.DType _ -> []
  | WT.DModule modul ->
      List.concat_map declarationStructureIssues modul.declarations
  | WT.DExpr expr -> exprIssues expr
  | WT.DTypeDB typ -> (
      match typ.definition with
      | WT.TDAlias _ -> []
      | WT.TDRecord _ | WT.TDEnum _ ->
          [ issue typ.range DBShape "[<DB>] type must be a type alias" ])
  | WT.DTest test -> (
      exprIssues test.actual
      @
      match test.expected with
      | WT.TEExpr expr -> exprIssues expr
      | WT.TEError _ | WT.TESqlError _ -> [])

(*
   Check mode-independent invariants required by WrittenTypes lowering.
*)
let validateStructure (sourceFile : WT.sourceFile) =
  List.concat_map declarationStructureIssues sourceFile.declarations
  @ List.concat_map exprIssues sourceFile.exprsToEval

(*
   WrittenTypes does not distinguish a file module header (`module A.B`),
   which may be empty, from a block module (`module X =`), which may not. The
   parser validates the block form while it still has that syntax detail.
*)
let rec declarationPurposeIssues mode = function
  | WT.DFunction _ | WT.DValue _ | WT.DType _ -> []
  | WT.DModule modul ->
      List.concat_map (declarationPurposeIssues mode) modul.declarations
  | WT.DExpr expr -> (
      match mode with
      | Package ->
          [
            issue (WT.exprRange expr) PackageExpression
              "Expressions are not allowed in package files";
          ]
      | Test ->
          [
            issue (WT.exprRange expr) TestAssertion
              "Test expressions must use 'actual = expected'";
          ]
      | Script -> [])
  | WT.DTypeDB typ -> (
      match mode with
      | Test -> []
      | Script | Package ->
          [
            issue typ.range DBMode
              "[<DB>] declarations are only allowed in test files";
          ])
  | WT.DTest test -> (
      match mode with
      | Test -> []
      | Script | Package ->
          [
            issue test.range TestMode
              "Test assertions are only allowed in test files";
          ])

(*
   Check only the rules that depend on whether the source is a script, package,
   or test file.
*)
let validatePurpose mode (sourceFile : WT.sourceFile) =
  let declarations =
    List.concat_map (declarationPurposeIssues mode) sourceFile.declarations
  in
  let trailing =
    List.concat_map
      (fun expr ->
        match mode with
        | Package ->
            [
              issue (WT.exprRange expr) PackageExpression
                "Expressions are not allowed in package files";
            ]
        | Test ->
            [
              issue (WT.exprRange expr) TestAssertion
                "Test expressions must use 'actual = expected'";
            ]
        | Script -> [])
      sourceFile.exprsToEval
  in
  declarations @ trailing

(*
   Validate every pre-lowering rule and return an opaque wrapper on success.
*)
let validate mode sourceFile =
  match validateStructure sourceFile @ validatePurpose mode sourceFile with
  | [] -> Ok (ValidatedSourceFile (mode, sourceFile))
  | first :: rest -> Error (ParserDependencies.ofList first rest)
