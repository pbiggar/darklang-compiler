(*
   Function annotations may introduce type parameters without listing them
   after the function name. Keep their first-seen order for positional calls.
   The success payload does not exist for these constructors.
   TNever keeps the type precise while the runtime reports the failed unwrap.
   Check annotated, nongeneric functions and sequential values directly from
   WrittenTypes. The production entry point is switched only after declaration
   catalogs, recursion, matches, and generic checking are included.
*)
(*
   Resolve the type syntax while retaining the compiler's nominal registry as
   the authority for custom names. No source AST type is constructed here.
   Equality and dictionary keys use the field layout, even when source record
   declarations have different names. Keep ordinary assignments nominal.
   Predeclare annotated functions before checking bodies, so calls use stable
   identities regardless of source order. The nominal registry will replace
   the restricted resolver as type declarations are brought into this path.
*)
(* WrittenChecking.ml - Construct checked source from validated interpreter syntax. *)
(* Declarations retained by a checked source batch for separately checked
   source units. The representation stays inside this direct checker. *)
[@@@warning "-4"]
type environment = WrittenDeclarations.environment
let includeAllocatedFunctions = WrittenDeclarations.includeAllocatedFunctions
(* This first checking slice accepts one closed entry expression. It provides
   a direct WrittenTypes-to-CheckedAST path while declaration checking grows. *)
let checkClosedProgram validated =
 Result.bind (WrittenSource.items validated) (function
 | [WrittenSource.Expression ([], expression)] ->
  Result.map (fun (typ, expression, symbols) -> typ, CheckedAST.programFromCheckedParts (symbols, [CheckedAST.Expression expression]))
   (WrittenExpressions.checkExpression WrittenTypeSupport.emptyGlobals StringOrder.Map.empty (CheckedAST.emptySymbols ()) None expression)
 | _ -> Error "Closed checking requires a single unscoped entry expression")
let checkSimpleProgram requireEntry validated =
 Result.map (fun (typ, program, _) -> typ, program)
  (Result.bind (WrittenSource.items validated) (WrittenDeclarations.checkItems WrittenExpressions.checkExpression None false requireEntry))
let checkSourceUnitsWithBase base allowInternal requireEntry units =
 Result.bind (Result.map List.concat (ResultList.traverse WrittenSource.items units))
  (WrittenDeclarations.checkItems WrittenExpressions.checkExpression base allowInternal requireEntry)
let checkSourceUnits allowInternal requireEntry units =
 Result.map (fun (typ, program, _) -> typ, program) (checkSourceUnitsWithBase None allowInternal requireEntry units)
(* Publish the checked declarations to the later compiler passes. This reads
   the completed checked program; source checking has already finished. *)
let typeCheckEnvironment = WrittenEnvironment.typeCheckEnvironment
