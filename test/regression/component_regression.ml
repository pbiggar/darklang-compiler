(* component_regression.ml - Check component equivalence and bounded stack usage. *)
open Dark_compiler
module S = Set.Make (Int)

let require condition message = if not condition then failwith message

let reach edges root =
  let rec visit seen = function
    | [] -> seen
    | vertex :: rest when S.mem vertex seen -> visit seen rest
    | vertex :: rest -> visit (S.add vertex seen) (edges.(vertex) @ rest)
  in
  visit S.empty edges.(root)

let check edges =
  let actual = StronglyConnectedComponents.classify edges in
  let closures = Array.init (Array.length edges) (reach edges) in
  Array.iteri
    (fun left _ ->
      Array.iteri
        (fun right _ ->
          let expected =
            left = right
            || (S.mem right closures.(left) && S.mem left closures.(right))
          in
          require
            (actual.(left) = actual.(right) = expected)
            "Component differs from mutual reachability")
        edges)
    edges

let () =
  List.iter check
    [
      [||];
      [| [] |];
      [| [ 0 ] |];
      [| [ 1 ]; [ 2 ]; [ 0 ]; [ 2 ]; [] |];
      [| [ 1; 1 ]; [ 0 ]; [ 3 ]; [ 2 ] |];
    ];
  let random = Random.State.make [| 7719 |] in
  for count = 1 to 75 do
    for _ = 1 to 10 do
      check
        (Array.init count (fun _ ->
             List.init count Fun.id
             |> List.filter (fun _ -> Random.State.int random 8 = 0)))
    done
  done;
  let chain =
    Array.init 50000 (fun vertex ->
        if vertex = 49999 then [] else [ vertex + 1 ])
  in
  let isolated = StronglyConnectedComponents.classify chain in
  require
    (List.length (List.sort_uniq Int.compare (Array.to_list isolated)) = 50000)
    "Chain components collapsed";
  chain.(49999) <- [ 0 ];
  let cycle = StronglyConnectedComponents.classify chain in
  require
    (Array.for_all (( = ) cycle.(0)) cycle)
    "Long cycle was not one component";
  for bit = 0 to 63 do
    let word = Int64.shift_left 1L bit in
    require
      (Bitset.indicesToList [| word |] = [ bit ] && Bitset.count [| word |] = 1)
      "Bitset intrinsic differs"
  done;
  require
    (Bitset.count [| Int64.minus_one; 0L |] = 64)
    "Negative bitset word count differs";
  print_endline
    "Component equivalence, 50k-node stack bounds and bitset regressions passed"
