(* foundation_probe.ml - Observe bitsets, result traversal, and ELF placement. *)
open Dark_compiler

let integers xs = "[" ^ String.concat "," (List.map string_of_int xs) ^ "]"
let words xs =
  "[" ^ String.concat "," (Array.to_list (Array.map (Printf.sprintf "\"%016Lx\"") xs)) ^ "]"
let observe bits =
  Printf.sprintf "[%s,%s,%d]" (words bits) (integers (Bitset.indicesToList bits)) (Bitset.count bits)

let run () =
  List.iter
    (fun bitCount ->
      for seed = 0 to 31 do
        let left = Bitset.empty (Bitset.wordCount bitCount) in
        let right = Bitset.empty (Bitset.wordCount bitCount) in
        for index = 0 to bitCount - 1 do
          if (index * 17 + seed) mod 5 = 0 then Bitset.addIndexInPlace index left;
          if (index * 11 + seed) mod 7 = 0 then Bitset.addIndexInPlace index right
        done;
        let merged = Bitset.union left right in
        let difference = Bitset.diff left right in
        let intersection = Bitset.intersectMany left [right] in
        let mutableUnion = Bitset.clone left in
        Bitset.unionInPlace mutableUnion right;
        let mutableDiff = Bitset.clone left in
        Bitset.diffInPlace mutableDiff right;
        let mutableIntersection = Bitset.clone left in
        Bitset.intersectInPlace mutableIntersection right;
        Printf.printf "[%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s]\n"
          bitCount seed (observe left) (observe right) (observe (Bitset.all bitCount))
          (observe merged) (observe difference) (observe intersection)
          (observe mutableUnion) (observe mutableDiff) (observe mutableIntersection)
      done)
    [0; 1; 7; 63; 64; 65; 127; 128; 129; 257; 1024];
  List.iter
    (fun endOffset -> Printf.printf "[%d,%d]\n" endOffset (RuntimeDataLayout.elfCounterOffset endOffset))
    [0; 1; 65535; 65536; 65537; 1048576];
  let events = ref [] in
  let result = ResultList.mapResults
      (fun value -> events := value :: !events;
        if value = 4 then Error "stop" else Ok (value * 2)) [1; 2; 3; 4; 5] in
  Printf.printf "[%s,%b]\n" (integers (List.rev !events)) (result = Error "stop");
  let collected = ResultList.collectResults (fun value -> Ok [value; value + 10]) [1; 2; 3] in
  match collected with
  | Ok values -> Printf.printf "%s\n" (integers values)
  | Error _ -> Crash.crash "Foundation result collector failed"
