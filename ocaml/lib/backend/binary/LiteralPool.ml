(*
   Pools are frozen once in first-use order; reverse indexes deduplicate values.
*)
(* LiteralPool.fs - Dense literal storage for late constant resolution. *)
module FloatBitsMap = Map.Make (Int64)
type stringPool = {strings : (string * int) array; stringToId : int StringOrder.Map.t}
(*
   Exact IEEE-754 bits distinguish signed zero and NaN payloads.
*)
type floatPool = {floats : float array; floatBitsToId : int FloatBitsMap.t}
let emptyStringPool = {strings = [||]; stringToId = StringOrder.Map.empty}
let emptyFloatPool = {floats = [||]; floatBitsToId = FloatBitsMap.empty}
let increment value = Int32.to_int (Int32.add (Int32.of_int value) 1l)
let utf8Length value =
 let units = HostText.utf16Units value in
 let rec count index length = if index = Array.length units then length else
  let value = units.(index) in
  if value >= 0xd800 && value <= 0xdbff && index + 1 < Array.length units && units.(index + 1) >= 0xdc00 && units.(index + 1) <= 0xdfff then count (index + 2) (length + 4)
  else count (index + 1) (length + if value < 0x80 then 1 else if value < 0x800 then 2 else 3) in count 0 0
(*
   Build in first-use order without copying a growing array for every literal.
*)
let createStringPool values =
 let entries, ids, _ = Seq.fold_left (fun (entries, ids, next) value -> if StringOrder.Map.mem value ids then entries, ids, next else (value, utf8Length value) :: entries, StringOrder.Map.add value next ids, increment next) ([], StringOrder.Map.empty, 0) values in
 {strings = Array.of_list (List.rev entries); stringToId = ids}
let createFloatPool values =
 let entries, ids, _ = Seq.fold_left (fun (entries, ids, next) value -> let bits = Int64.bits_of_float value in if FloatBitsMap.mem bits ids then entries, ids, next else value :: entries, FloatBitsMap.add bits next ids, increment next) ([], FloatBitsMap.empty, 0) values in
 {floats = Array.of_list (List.rev entries); floatBitsToId = ids}
