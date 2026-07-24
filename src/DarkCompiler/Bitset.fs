// Bitset.fs - Low-level bitset utilities for compiler passes
//
// Provides allocation-friendly bitset operations used by register allocation
// and dominance analysis. The representation is a raw uint64 array.

module Bitset

type Bitset = uint64 array

let wordCount (bitCount: int) : int =
    (bitCount + 63) / 64

let empty (wordCount: int) : Bitset =
    Array.zeroCreate<uint64> wordCount

let private requireIndexInRange (operation: string) (wordCount: int) (idx: int) : unit =
    let bitCapacity = wordCount * 64
    if idx < 0 || idx >= bitCapacity then
        Crash.crash $"{operation} index {idx} is outside bitset capacity {bitCapacity}"

let private requireMatchingWordCount (operation: string) (left: Bitset) (right: Bitset) : unit =
    if left.Length <> right.Length then
        Crash.crash $"Bitset {operation} requires matching word counts"

let all (bitCount: int) : Bitset =
    let wordCount = wordCount bitCount
    if wordCount = 0 then
        [||]
    else
        let extraBits = bitCount % 64
        let lastMask =
            if extraBits = 0 then System.UInt64.MaxValue
            else (1UL <<< extraBits) - 1UL
        Array.init wordCount (fun i ->
            if i = wordCount - 1 then lastMask else System.UInt64.MaxValue)

let singleton (wordCount: int) (idx: int) : Bitset =
    requireIndexInRange "Bitset.singleton" wordCount idx
    if wordCount = 0 then
        [||]
    else
        let word = idx >>> 6
        let bit = 1UL <<< (idx &&& 63)
        Array.init wordCount (fun i ->
            if i = word then bit else 0UL)

let clone (bits: Bitset) : Bitset =
    Array.copy bits

let isEmpty (bits: Bitset) : bool =
    bits |> Array.forall (fun word -> word = 0UL)

let equal (left: Bitset) (right: Bitset) : bool =
    requireMatchingWordCount "equality" left right
    Array.forall2 (=) left right

let union (left: Bitset) (right: Bitset) : Bitset =
    requireMatchingWordCount "union" left right
    if isEmpty left then
        right
    else if isEmpty right then
        left
    else
        Array.init left.Length (fun i -> left.[i] ||| right.[i])

let diff (left: Bitset) (right: Bitset) : Bitset =
    requireMatchingWordCount "difference" left right
    if isEmpty right then
        left
    else
        Array.init left.Length (fun i -> left.[i] &&& (~~~right.[i]))

let intersectMany (first: Bitset) (rest: Bitset list) : Bitset =
    let wordCount = first.Length
    rest |> List.iter (requireMatchingWordCount "intersection" first)
    Array.init wordCount (fun i ->
        rest |> List.fold (fun acc set -> acc &&& set.[i]) first.[i])

let containsIndex (idx: int) (bits: Bitset) : bool =
    let wordIdx = idx >>> 6
    let bitIdx = idx &&& 63
    if wordIdx < bits.Length then
        (bits.[wordIdx] &&& (1UL <<< bitIdx)) <> 0UL
    else
        false

let addIndexInPlace (idx: int) (bits: Bitset) : unit =
    requireIndexInRange "Bitset.addIndexInPlace" bits.Length idx
    let wordIdx = idx >>> 6
    let bitIdx = idx &&& 63
    if wordIdx < bits.Length then
        bits.[wordIdx] <- bits.[wordIdx] ||| (1UL <<< bitIdx)

let add (idx: int) (bits: Bitset) : Bitset =
    if bits.Length = 0 then
        bits
    else
        let updated = Array.copy bits
        addIndexInPlace idx updated
        updated

let removeIndexInPlace (idx: int) (bits: Bitset) : unit =
    requireIndexInRange "Bitset.removeIndexInPlace" bits.Length idx
    let wordIdx = idx >>> 6
    let bitIdx = idx &&& 63
    if wordIdx < bits.Length then
        bits.[wordIdx] <- bits.[wordIdx] &&& (~~~(1UL <<< bitIdx))

let unionInPlace (left: Bitset) (right: Bitset) : unit =
    requireMatchingWordCount "union" left right
    for i in 0 .. left.Length - 1 do
        left.[i] <- left.[i] ||| right.[i]

let intersectInPlace (left: Bitset) (right: Bitset) : unit =
    requireMatchingWordCount "intersection" left right
    for i in 0 .. left.Length - 1 do
        left.[i] <- left.[i] &&& right.[i]

let diffInPlace (left: Bitset) (right: Bitset) : unit =
    requireMatchingWordCount "difference" left right
    for i in 0 .. left.Length - 1 do
        left.[i] <- left.[i] &&& (~~~right.[i])

let intersects (left: Bitset) (right: Bitset) : bool =
    requireMatchingWordCount "intersection" left right
    let mutable found = false
    let mutable i = 0
    while not found && i < left.Length do
        if (left.[i] &&& right.[i]) <> 0UL then
            found <- true
        i <- i + 1
    found

let iterIndices (bits: Bitset) (f: int -> unit) : unit =
    for wordIdx in 0 .. bits.Length - 1 do
        let mutable word = bits.[wordIdx]
        let baseIdx = wordIdx * 64
        while word <> 0UL do
            let tz = System.Numerics.BitOperations.TrailingZeroCount word
            f (baseIdx + tz)
            word <- word &&& (word - 1UL)

let count (bits: Bitset) : int =
    let mutable total = 0
    for word in bits do
        let mutable w = word
        while w <> 0UL do
            w <- w &&& (w - 1UL)
            total <- total + 1
    total

let indicesToList (bits: Bitset) : int list =
    let mutable acc = []
    iterIndices bits (fun idx -> acc <- idx :: acc)
    List.rev acc
