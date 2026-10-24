(* NamedPackageTests.ml - Preserve scoped package tests for a separate execution adapter. *)
[@@@warning "-4"]

module WT = WrittenTypes
module Names = Set.Make (String)

type test = {
  modules : string list;
  qualifiedName : string;
  declaration : WT.testDecl;
}

type package = {
  declarations : Validation.validatedSourceFile;
  tests : test list;
}

let ( let* ) = Result.bind

let rec partitionDeclarations modules names declarations =
  let rec collect names kept tests = function
    | [] -> Ok (names, List.rev kept, List.rev tests)
    | WT.DTest declaration :: rest ->
        let qualifiedName =
          String.concat "." (modules @ [ declaration.WT.name.WT.name ])
        in
        if Names.mem qualifiedName names then
          Error ("Duplicate named package test '" ^ qualifiedName ^ "'")
        else
          collect
            (Names.add qualifiedName names)
            kept
            ({ modules; qualifiedName; declaration } :: tests)
            rest
    | WT.DModule modul :: rest ->
        let* segments = WrittenSource.moduleSegments (snd modul.WT.name) in
        let* names, declarations, nested =
          partitionDeclarations (modules @ segments) names modul.WT.declarations
        in
        collect names
          (WT.DModule { modul with WT.declarations } :: kept)
          (List.rev_append nested tests)
          rest
    | declaration :: rest -> collect names (declaration :: kept) tests rest
  in
  collect names [] [] declarations

let partition validated =
  if Validation.ValidatedSourceFile.mode validated <> Validation.Package then
    Error "Named package test extraction requires Package source"
  else
    let source = Validation.ValidatedSourceFile.toWrittenTypes validated in
    let* _, declarations, tests =
      partitionDeclarations [] Names.empty source.WT.declarations
    in
    match
      Validation.validate Validation.Package { source with WT.declarations }
    with
    | Ok declarations -> Ok { declarations; tests }
    | Error _ ->
        Crash.crash
          "Removing named tests invalidated an already validated package"

let executable validated qualifiedName =
  let* package = partition validated in
  match
    List.find_opt
      (fun test -> String.equal test.qualifiedName qualifiedName)
      package.tests
  with
  | None -> Error ("Unknown named package test '" ^ qualifiedName ^ "'")
  | Some test -> (
      let rec select modules declarations =
        Result.map List.concat
          (ResultList.traverse
             (function
               | WT.DTest declaration ->
                   let name =
                     String.concat "."
                       (modules @ [ declaration.WT.name.WT.name ])
                   in
                   Ok
                     (if String.equal name qualifiedName then
                        [ WT.DExpr declaration.WT.body ]
                      else [])
               | WT.DModule modul ->
                   let* segments =
                     WrittenSource.moduleSegments (snd modul.WT.name)
                   in
                   let* declarations =
                     select (modules @ segments) modul.WT.declarations
                   in
                   Ok [ WT.DModule { modul with WT.declarations } ]
               | declaration -> Ok [ declaration ])
             declarations)
      in
      let source = Validation.ValidatedSourceFile.toWrittenTypes validated in
      let* declarations = select [] source.WT.declarations in
      match
        Validation.validate Validation.Script { source with WT.declarations }
      with
      | Ok executable -> Ok (executable, test)
      | Error _ ->
          Crash.crash
            "Selecting a validated package test invalidated its source")
