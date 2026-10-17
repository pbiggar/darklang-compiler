(* AsyncListCalls.ml - Identify list builders that combine element interfaces without inspecting them. *)
type argument = ListInput | ElementInput
type builder = { callee : string; arguments : argument list }

let builder name =
  let operations =
    [
      ("singleton", "__singleton", [ ElementInput ]);
      ("__singleton", "__singleton", [ ElementInput ]);
      ("push", "__push", [ ListInput; ElementInput ]);
      ("__push", "__push", [ ListInput; ElementInput ]);
      ("pushBack", "__pushBack", [ ListInput; ElementInput ]);
      ("__pushBack", "__pushBack", [ ListInput; ElementInput ]);
      ("append", "__concat", [ ListInput; ListInput ]);
      ("__concat", "__concat", [ ListInput; ListInput ]);
    ]
  in
  List.find_map
    (fun (source, target, arguments) ->
      let prefix = "Darklang.Stdlib.List." ^ source ^ "_" in
      if String.starts_with ~prefix name then
        Some { callee = "Darklang.Stdlib.List." ^ target; arguments }
      else None)
    operations
