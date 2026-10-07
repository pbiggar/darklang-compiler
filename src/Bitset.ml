(*
   Bitset.ml - Low-level bitset utilities for compiler passes
   Provides allocation-friendly bitset operations used by register allocation
   and dominance analysis. The representation is a raw uint64 array.
*)
(* Bitset.ml - Allocation-conscious bitsets for allocation and dominance. *)
type bitset = int64 array

let wordCount bitCount = (bitCount + 63) / 64
let empty words = Array.make words 0L

let requireIndexInRange operation words idx =
  let capacity = words * 64 in
  if idx < 0 || idx >= capacity then
    Crash.crash
      (Printf.sprintf "%s index %d is outside bitset capacity %d" operation idx
         capacity)

let requireMatchingWordCount operation left right =
  if Array.length left <> Array.length right then
    Crash.crash ("Bitset " ^ operation ^ " requires matching word counts")

let all bitCount =
  let words = wordCount bitCount in
  if words = 0 then [||]
  else
    let extraBits = bitCount mod 64 in
    let lastMask =
      if extraBits = 0 then Int64.minus_one
      else Int64.sub (Int64.shift_left 1L extraBits) 1L
    in
    Array.init words (fun i ->
        if i = words - 1 then lastMask else Int64.minus_one)

let singleton words idx =
  requireIndexInRange "Bitset.singleton" words idx;
  if words = 0 then [||]
  else
    let word = idx lsr 6 in
    let bit = Int64.shift_left 1L (idx land 63) in
    Array.init words (fun i -> if i = word then bit else 0L)

let clone = Array.copy
let isEmpty bits = Array.for_all (( = ) 0L) bits

let equal left right =
  requireMatchingWordCount "equality" left right;
  Array.for_all2 Int64.equal left right

let union left right =
  requireMatchingWordCount "union" left right;
  if isEmpty left then right
  else if isEmpty right then left
  else Array.init (Array.length left) (fun i -> Int64.logor left.(i) right.(i))

let diff left right =
  requireMatchingWordCount "difference" left right;
  if isEmpty right then left
  else
    Array.init (Array.length left) (fun i ->
        Int64.logand left.(i) (Int64.lognot right.(i)))

let intersectMany first rest =
  List.iter (requireMatchingWordCount "intersection" first) rest;
  Array.init (Array.length first) (fun i ->
      List.fold_left (fun acc bits -> Int64.logand acc bits.(i)) first.(i) rest)

let containsIndex idx bits =
  let wordIdx = idx lsr 6 in
  let bitIdx = idx land 63 in
  idx >= 0
  && wordIdx < Array.length bits
  && Int64.logand bits.(wordIdx) (Int64.shift_left 1L bitIdx) <> 0L

let addIndexInPlace idx bits =
  requireIndexInRange "Bitset.addIndexInPlace" (Array.length bits) idx;
  let wordIdx = idx lsr 6 in
  let bitIdx = idx land 63 in
  if wordIdx < Array.length bits then
    bits.(wordIdx) <- Int64.logor bits.(wordIdx) (Int64.shift_left 1L bitIdx)

let add idx bits =
  requireIndexInRange "Bitset.add" (Array.length bits) idx;
  let updated = Array.copy bits in
  addIndexInPlace idx updated;
  updated

let removeIndexInPlace idx bits =
  requireIndexInRange "Bitset.removeIndexInPlace" (Array.length bits) idx;
  let wordIdx = idx lsr 6 in
  let bitIdx = idx land 63 in
  if wordIdx < Array.length bits then
    bits.(wordIdx) <-
      Int64.logand bits.(wordIdx) (Int64.lognot (Int64.shift_left 1L bitIdx))

let unionInPlace left right =
  requireMatchingWordCount "union" left right;
  for i = 0 to Array.length left - 1 do
    left.(i) <- Int64.logor left.(i) right.(i)
  done

let intersectInPlace left right =
  requireMatchingWordCount "intersection" left right;
  for i = 0 to Array.length left - 1 do
    left.(i) <- Int64.logand left.(i) right.(i)
  done

let diffInPlace left right =
  requireMatchingWordCount "difference" left right;
  for i = 0 to Array.length left - 1 do
    left.(i) <- Int64.logand left.(i) (Int64.lognot right.(i))
  done

let intersects left right =
  requireMatchingWordCount "intersection" left right;
  Array.exists2 (fun a b -> Int64.logand a b <> 0L) left right

(* Use the OCaml integer bit-count intrinsic. Scanning by
   recursive boxed shifts allocated for every zero bit in allocation graphs. *)
let trailingZeroCount = Int64.trailing_zeros

let iterIndices bits f =
  Array.iteri
    (fun wordIdx initial ->
      let word = ref initial in
      let baseIdx = wordIdx * 64 in
      while !word <> 0L do
        f (baseIdx + trailingZeroCount !word);
        word := Int64.logand !word (Int64.sub !word 1L)
      done)
    bits

let count bits =
  Array.fold_left (fun total word -> total + Int64.popcount word) 0 bits

let indicesToList bits =
  let acc = ref [] in
  iterIndices bits (fun idx -> acc := idx :: !acc);
  List.rev !acc
