(* Reducer.ml - Deterministic syntax reductions checked against the semantic oracle. *)
open Dark_compiler
module W = WrittenTypes
open! W
open! Tokenizer

(* Lexer columns count Unicode scalars. Splicing uses their original byte offsets. *)
let offsets source =
  let positions = Hashtbl.create (String.length source) in
  let row = ref 0 and column = ref 0 in
  Uutf.String.fold_utf_8
    (fun () offset decoded ->
      Hashtbl.add positions (!row, !column) offset;
      match decoded with
      | `Uchar character when Uchar.to_int character = 10 ->
          incr row;
          column := 0
      | `Uchar _ | `Malformed _ -> incr column)
    () source;
  Hashtbl.add positions (!row, !column) (String.length source);
  fun (position : Tokenizer.pos) ->
    match Hashtbl.find_opt positions (position.row, position.column) with
    | Some offset -> offset
    | None -> Crash.crash "Fuzzer reduction range was outside source"

let children = function
  | W.EInfix (_, _, left, right) | W.EStatement (_, left, right) ->
      [ left; right ]
  | W.ELet (_, _, value, body, _, _) -> [ value; body ]
  | W.EIf (_, condition, yes, no, _, _, _) ->
      condition :: yes :: Option.to_list no
  | W.EApply (_, func, _, arguments) -> func :: arguments
  | W.EList (_, elements, _, _) -> List.map fst elements
  | W.ETuple (_, first, _, second, rest, _, _) ->
      first :: second :: List.map snd rest
  | W.ERecordFieldAccess (_, value, _, _) | W.ELambda (_, _, value, _, _) ->
      [ value ]
  | W.ERecord (_, _, fields, _, _) ->
      List.map (fun (_, _, value) -> value) fields
  | W.ERecordUpdate (_, value, fields, _, _, _) ->
      value :: List.map (fun (_, _, field) -> field) fields
  | W.EDict (_, entries, _, _, _) ->
      List.concat_map (fun (_, key, _, value) -> [ key; value ]) entries
  | W.EEnum (_, _, _, arguments, _) -> arguments
  | W.EMatch (_, value, cases, _, _) ->
      value
      :: List.concat_map
           (fun (case : W.matchCase) ->
             Option.to_list (Option.map snd case.whenCondition) @ [ case.rhs ])
           cases
  | W.EString (_, _, segments, _, _) ->
      List.filter_map
        (function
          | W.StringInterpolation (_, value, _, _) -> Some value
          | W.StringText _ -> None)
        segments
  | _ -> []

let candidates source (parsed : W.sourceFile) =
  let offset = offsets source in
  let slice (range : W.range) =
    let first = offset range.start and last = offset range.end_ in
    String.sub source first (last - first)
  in
  let replace (range : W.range) value =
    let first = offset range.start and last = offset range.end_ in
    String.sub source 0 first ^ value
    ^ String.sub source last (String.length source - last)
  in
  let rec expression value =
    let range = W.exprRange value in
    let nested = children value in
    let replacements =
      List.map (fun child -> "(" ^ slice (W.exprRange child) ^ ")") nested
    in
    let literals =
      match value with
      | W.EBool _ -> [ "false"; "true" ]
      | W.EInt64 _ -> [ "0L"; "1L" ]
      | W.EInt _ -> [ "0I" ]
      | W.EString _ -> [ "\"\"" ]
      | W.EUnit _ -> []
      | _ -> [ "0L"; "false" ]
    in
    let drops =
      match value with
      | W.EMatch (_, _, cases, _, _) ->
          List.concat_map
            (fun (case : W.matchCase) ->
              let arm =
                {
                  Tokenizer.start = case.barRange.start;
                  end_ = (W.exprRange case.rhs).end_;
                }
              in
              let guard =
                match case.whenCondition with
                | None -> []
                | Some (keyword, condition) ->
                    [
                      replace
                        {
                          Tokenizer.start = keyword.start;
                          end_ = (W.exprRange condition).end_;
                        }
                        "";
                    ]
              in
              replace arm "" :: guard)
            cases
      | W.EDict (_, entries, _, _, _) ->
          (* Whole-entry removals are parsed again; separators belong to the syntax. *)
          List.filter_map
            (fun (_, key, _, value) ->
              let range =
                {
                  Tokenizer.start = (W.exprRange key).start;
                  end_ = (W.exprRange value).end_;
                }
              in
              let first = offset range.start and last = offset range.end_ in
              if last < String.length source && source.[last] = ',' then
                Some
                  (String.sub source 0 first
                  ^ String.sub source (last + 1)
                      (String.length source - last - 1))
              else None)
            entries
      | _ -> []
    in
    List.map (replace range) (replacements @ literals)
    @ drops
    @ List.concat_map expression nested
  in
  let rec declaration = function
    | W.DFunction func -> replace func.range "" :: expression func.body
    | W.DValue value -> replace value.range "" :: expression value.body
    | W.DType typ | W.DTypeDB typ -> [ replace typ.range "" ]
    | W.DModule module_ -> List.concat_map declaration module_.declarations
    | W.DExpr value -> expression value
    | W.DTest _ -> []
  in
  let candidates =
    List.concat_map declaration parsed.W.declarations
    @ List.concat_map expression parsed.exprsToEval
  in
  candidates
  |> List.filter (fun value -> String.length value < String.length source)
  |> List.sort_uniq (fun left right ->
      let size = Int.compare (String.length left) (String.length right) in
      if size = 0 then String.compare left right else size)

let minimize check source =
  let original = check source in
  match original with
  | Oracle.Passed -> Error "Input does not reproduce a discrepancy"
  | Oracle.Unsupported _ | Oracle.OracleFailed _ ->
      Error "Interpreter must accept the minimizer input"
  | Oracle.CompilerRejected _ | Oracle.CompilerCrashed _ | Oracle.NativeFailed _
  | Oracle.ResultMismatch _ ->
      let rec reduce source outcome attempts reductions =
        match WrittenParsing.parse Validation.Script source with
        | Error message -> Error ("Cannot parse minimizer input: " ^ message)
        | Ok validated ->
            let parsed =
              Validation.ValidatedSourceFile.toWrittenTypes validated
            in
            let rec tryCandidates attempts = function
              | [] -> Ok (source, outcome, attempts, reductions)
              | candidate :: rest -> (
                  match WrittenParsing.parse Validation.Script candidate with
                  | Error _ -> tryCandidates attempts rest
                  | Ok _ ->
                      let result = check candidate in
                      if Oracle.sameFailure original result then
                        reduce candidate result (attempts + 1) (reductions + 1)
                      else tryCandidates (attempts + 1) rest)
            in
            tryCandidates attempts (candidates source parsed)
      in
      reduce source original 0 0
