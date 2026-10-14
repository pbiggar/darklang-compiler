(* WrittenPipeSupport.ml - Lower pipelines once for checking and continuation inference. *)
module WT = WrittenTypes
open WT

let lower range first segments =
  List.fold_left
    (fun input (_, segment) ->
      match segment with
      | WT.EPipeInfix (_, op, right) -> WT.EInfix (range, op, input, right)
      | WT.EPipeFnCall (_, name, typeArgs, args) ->
          WT.EApply
            (range, WT.EFnName (name.range, name), typeArgs, input :: args)
      | WT.EPipeEnum (_, name, caseName, fields, dot) ->
          WT.EEnum (range, name, caseName, input :: fields, dot)
      | WT.EPipeLambda (_, [ pattern ], body, keyword, arrow) ->
          WT.ELet (range, pattern, input, body, keyword, arrow)
      | WT.EPipeLambda (_, patterns, body, keyword, arrow) ->
          WT.EApply
            ( range,
              WT.ELambda (range, patterns, body, keyword, arrow),
              [],
              [ input ] )
      | WT.EPipeVariableOrFnCall (nameRange, name) ->
          let identifier : WT.identifier = { range = nameRange; name } in
          let qualified : WT.qualifiedFnIdentifier =
            { range = nameRange; modules = []; fn = identifier }
          in
          WT.EApply (range, WT.EFnName (nameRange, qualified), [], [ input ]))
    first segments
