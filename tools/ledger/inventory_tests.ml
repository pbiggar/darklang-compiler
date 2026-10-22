(* inventory_tests.ml - Read fixture identities and per-test annotations through the actual runner parser. *)
module E = E2EFormat
module J = Yojson.Basic.Util

let () =
  match Array.to_list Sys.argv with
  | [ _; manifest; output ] ->
      let paths =
        Yojson.Basic.from_file manifest |> J.to_list |> List.map J.to_string
      in
      let rows =
        List.map
          (fun path ->
            match E.parseE2ETestFile path with
            | Error message ->
                `Assoc [ ("source", `String path); ("error", `String message) ]
            | Ok tests ->
                `Assoc
                  [
                    ("source", `String path);
                    ( "tests",
                      `List
                        (List.map
                           (fun (test : E.e2eTest) ->
                             `Assoc
                               [
                                 ("name", `String test.E.name);
                                 ("line", `Int test.E.sourceLine);
                                 ( "skip",
                                   match test.E.skipReason with
                                   | None -> `Null
                                   | Some reason -> `String reason );
                               ])
                           tests) );
                  ])
          paths
      in
      Yojson.Basic.to_file output (`List rows)
  | _ ->
      prerr_endline "Usage: inventory_tests MANIFEST OUTPUT";
      exit 1
