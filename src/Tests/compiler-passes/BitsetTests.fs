// BitsetTests.fs - Unit tests for low-level bitset utilities.
//
// These tests cover invariants that compiler dataflow passes rely on when
// mapping dense labels and virtual-register ids into bitset storage.

module BitsetTests

type TestResult = Result<unit, string>

let private expectCrash (name: string) (action: unit -> unit) : TestResult =
    try
        action ()
        Error $"Expected {name} to crash for an out-of-range index"
    with
    | _ -> Ok ()

let private expectCrashMessage (name: string) (expectedMessage: string) (action: unit -> unit) : TestResult =
    try
        action ()
        Error $"Expected {name} to crash with: {expectedMessage}"
    with
    | ex when ex.Message = expectedMessage -> Ok ()
    | ex -> Error $"Expected {name} to crash with '{expectedMessage}', got: {ex.Message}"

let testAddIndexInPlaceRejectsOutOfRangeIndex () : TestResult =
    let bits = Bitset.empty 1
    expectCrash "addIndexInPlace" (fun () -> Bitset.addIndexInPlace 64 bits)

let testAddRejectsOutOfRangeIndex () : TestResult =
    let bits = Bitset.empty 0
    expectCrash "add" (fun () -> Bitset.add 0 bits |> ignore)

let testRemoveIndexInPlaceRejectsOutOfRangeIndex () : TestResult =
    let bits = Bitset.all 64
    expectCrash "removeIndexInPlace" (fun () -> Bitset.removeIndexInPlace 64 bits)

let testSingletonRejectsOutOfRangeIndex () : TestResult =
    expectCrash "singleton" (fun () -> Bitset.singleton 1 64 |> ignore)

let testContainsIndexHandlesIndexBounds () : TestResult =
    let bits = Bitset.singleton 1 0
    if not (Bitset.containsIndex 0 bits) then
        Error "Expected containsIndex to find a valid present index"
    else if Bitset.containsIndex 64 bits then
        Error "Expected containsIndex to return false for an oversized index"
    else if Bitset.containsIndex -1 bits then
        Error "Expected containsIndex to return false for a negative index"
    else
        Ok ()

let testIntersectManyRejectsMismatchedWordCounts () : TestResult =
    let first = Bitset.empty 2
    let shorter = Bitset.empty 1
    expectCrashMessage
        "intersectMany"
        "Bitset intersection requires matching word counts"
        (fun () -> Bitset.intersectMany first [shorter] |> ignore)

let tests = [
    ("addIndexInPlace rejects out-of-range index", testAddIndexInPlaceRejectsOutOfRangeIndex)
    ("add rejects out-of-range index", testAddRejectsOutOfRangeIndex)
    ("removeIndexInPlace rejects out-of-range index", testRemoveIndexInPlaceRejectsOutOfRangeIndex)
    ("singleton rejects out-of-range index", testSingletonRejectsOutOfRangeIndex)
    ("containsIndex handles index bounds", testContainsIndexHandlesIndexBounds)
    ("intersectMany rejects mismatched word counts", testIntersectManyRejectsMismatchedWordCounts)
]
