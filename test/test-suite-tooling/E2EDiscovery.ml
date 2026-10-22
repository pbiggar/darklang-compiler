(* E2EDiscovery.ml - Discover every fixture and retain its per-test source metadata. *)
module E = E2EFormat

let loadE2ETests files =
  let tests, errors =
    Array.fold_left
      (fun (tests, errors) file ->
        match E.parseE2ETestFile file with
        | Ok parsed -> (List.rev_append parsed tests, errors)
        | Error message -> (tests, (file, message) :: errors))
      ([], []) files
  in
  (Array.of_list (List.rev tests), List.rev errors)
