(* StronglyConnectedComponents.ml - Iterative Kosaraju traversal without recursive stacks. *)
let classify edges =
  let count = Array.length edges in
  let reverse = Array.make count [] in
  Array.iteri
    (fun source targets ->
      List.iter
        (fun target ->
          if target < 0 || target >= count then
            Crash.crash "Component edge is outside the graph";
          reverse.(target) <- source :: reverse.(target))
        targets)
    edges;
  let visited = Array.make count false and finished = ref [] in
  let rec visit = function
    | [] -> ()
    | (vertex, true) :: rest ->
        finished := vertex :: !finished;
        visit rest
    | (vertex, false) :: rest when visited.(vertex) -> visit rest
    | (vertex, false) :: rest ->
        visited.(vertex) <- true;
        visit
          (List.fold_left
             (fun stack target -> (target, false) :: stack)
             ((vertex, true) :: rest) edges.(vertex))
  in
  for vertex = 0 to count - 1 do
    if not visited.(vertex) then visit [ (vertex, false) ]
  done;
  let components = Array.make count (-1) and next = ref 0 in
  let rec assign component = function
    | [] -> ()
    | vertex :: rest when components.(vertex) >= 0 -> assign component rest
    | vertex :: rest ->
        components.(vertex) <- component;
        assign component
          (List.fold_left
             (fun stack source -> source :: stack)
             rest reverse.(vertex))
  in
  List.iter
    (fun vertex ->
      if components.(vertex) < 0 then (
        assign !next [ vertex ];
        incr next))
    !finished;
  components
