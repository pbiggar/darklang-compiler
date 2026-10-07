(*
   BitsetTests.ml - Unit tests for low-level bitset utilities.
   These tests cover invariants that compiler dataflow passes rely on when
   mapping dense labels and virtual-register ids into bitset storage.
*)
(* BitsetTests.ml - Port the original bitset bounds and shape invariants. *)
open Dark_compiler
type testResult = (unit, string) result

let expectCrash name action =
  match action () with
  | () -> Error ("Expected " ^ name ^ " to crash for an out-of-range index")
  | exception _ -> Ok ()

let expectCrashMessage name expected action =
  match action () with
  | () -> Error ("Expected " ^ name ^ " to crash with: " ^ expected)
  | exception Failure message when String.equal message expected -> Ok ()
  | exception ex ->
      Error (Printf.sprintf "Expected %s to crash with '%s', got: %s"
               name expected (Printexc.to_string ex))

let testAddIndexInPlaceRejectsOutOfRangeIndex () =
  let bits = Bitset.empty 1 in
  expectCrash "addIndexInPlace" (fun () -> Bitset.addIndexInPlace 64 bits)

let testAddRejectsOutOfRangeIndex () =
  let bits = Bitset.empty 0 in
  expectCrash "add" (fun () -> ignore (Bitset.add 0 bits))

let testRemoveIndexInPlaceRejectsOutOfRangeIndex () =
  let bits = Bitset.all 64 in
  expectCrash "removeIndexInPlace" (fun () -> Bitset.removeIndexInPlace 64 bits)

let testSingletonRejectsOutOfRangeIndex () =
  expectCrash "singleton" (fun () -> ignore (Bitset.singleton 1 64))

let testContainsIndexHandlesIndexBounds () =
  let bits = Bitset.singleton 1 0 in
  if not (Bitset.containsIndex 0 bits) then
    Error "Expected containsIndex to find a valid present index"
  else if Bitset.containsIndex 64 bits then
    Error "Expected containsIndex to return false for an oversized index"
  else if Bitset.containsIndex (-1) bits then
    Error "Expected containsIndex to return false for a negative index"
  else Ok ()

let testIntersectManyRejectsMismatchedWordCounts () =
  let first = Bitset.empty 2 in
  let shorter = Bitset.empty 1 in
  expectCrashMessage "intersectMany" "Bitset intersection requires matching word counts"
    (fun () -> ignore (Bitset.intersectMany first [shorter]))

let tests = [
  "addIndexInPlace rejects out-of-range index", testAddIndexInPlaceRejectsOutOfRangeIndex;
  "add rejects out-of-range index", testAddRejectsOutOfRangeIndex;
  "removeIndexInPlace rejects out-of-range index", testRemoveIndexInPlaceRejectsOutOfRangeIndex;
  "singleton rejects out-of-range index", testSingletonRejectsOutOfRangeIndex;
  "containsIndex handles index bounds", testContainsIndexHandlesIndexBounds;
  "intersectMany rejects mismatched word counts", testIntersectManyRejectsMismatchedWordCounts;
]
