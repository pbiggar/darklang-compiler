(* NameResolutionTests.ml - Original TypeCheckingTests filtered-resolution regression. *)
[@@@warning "-4-42"]
open Dark_compiler
(* Filtering must preserve same-spelled survivors and their import precedence. *)
let testFilteredResolutionCandidates () =
  let candidate owner = match NameResolution.candidate "SharedCase"
    (NameResolution.ConstructorSymbol (owner, "SharedCase")) (NameResolution.SourceDeclaration owner) with
    | Some candidate -> candidate | None -> Crash.crash "Test candidate has an invalid fixed spelling" in
  let first = candidate "First" and second = candidate "Second" in
  let environment = NameResolution.addCandidates [first; second] NameResolution.empty in
  let filtered = NameResolution.filterCandidates (fun (candidate : NameResolution.candidate) -> candidate.NameResolution.identity = first.NameResolution.identity) environment in
  let imported = NameResolution.merge filtered NameResolution.empty in
  let removed = NameResolution.filterCandidates (fun _ -> false) environment in
  match NameResolution.resolve NameResolution.Constructor "SharedCase" environment,
    NameResolution.resolve NameResolution.Constructor "SharedCase" filtered,
    NameResolution.resolve NameResolution.Constructor "SharedCase" imported,
    NameResolution.resolve NameResolution.Constructor "SharedCase" removed with
  | Error (NameResolution.AmbiguousReference _), Ok local, Ok imported, Error (NameResolution.UnresolvedName _)
    when local.NameResolution.identity = first.NameResolution.identity && local.NameResolution.provenance = first.NameResolution.provenance
      && imported.NameResolution.identity = first.NameResolution.identity && imported.NameResolution.provenance = NameResolution.PackageDeclaration "First" -> Ok ()
  | _ -> Error "Filtered/imported candidate resolution changed"
let tests = ["Filtered name-resolution candidates preserve survivors and imports", testFilteredResolutionCandidates]
