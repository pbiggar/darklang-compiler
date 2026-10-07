(*
   Pools are frozen once in first-use order; reverse indexes deduplicate values.
*)
(* LiteralPool.ml - Dense literal storage for late constant resolution. *)
module FloatBitsMap = Map.Make (Int64)

type stringPool = {
  strings : (string * int) array;
  stringToId : int StringOrder.Map.t;
}

(*
   Exact IEEE-754 bits distinguish signed zero and NaN payloads.
*)
type floatPool = { floats : float array; floatBitsToId : int FloatBitsMap.t }

let emptyStringPool = { strings = [||]; stringToId = StringOrder.Map.empty }
let emptyFloatPool = { floats = [||]; floatBitsToId = FloatBitsMap.empty }
let increment value = Int32.to_int (Int32.add (Int32.of_int value) 1l)
let utf8Length = String.length

(*
   Build in first-use order without copying a growing array for every literal.
*)
let createStringPool values =
  let entries, ids, _ =
    Seq.fold_left
      (fun (entries, ids, next) value ->
        if StringOrder.Map.mem value ids then (entries, ids, next)
        else
          ( (value, utf8Length value) :: entries,
            StringOrder.Map.add value next ids,
            increment next ))
      ([], StringOrder.Map.empty, 0)
      values
  in
  { strings = Array.of_list (List.rev entries); stringToId = ids }

let createFloatPool values =
  let entries, ids, _ =
    Seq.fold_left
      (fun (entries, ids, next) value ->
        let bits = Int64.bits_of_float value in
        if FloatBitsMap.mem bits ids then (entries, ids, next)
        else (value :: entries, FloatBitsMap.add bits next ids, increment next))
      ([], FloatBitsMap.empty, 0)
      values
  in
  { floats = Array.of_list (List.rev entries); floatBitsToId = ids }
