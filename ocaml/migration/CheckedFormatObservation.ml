[@@@warning "-4"]
open Dark_compiler
module C = InstrumentedCheckedAST
module W = InstrumentedWrittenChecking
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error message -> SemanticJson.union "FSharpResult" "Error" [str message]
let observeWith format source =
 let program value = C.programTopLevels value |> List.filter_map (function C.FunctionDef value -> Some value.C.body | C.ValueDef value -> Some value.C.body | C.Expression value -> Some value | C.TypeDef _ -> None) |> list (fun value -> str (format value)) in
 let sourceProgram source = result (fun (_, value, _) -> program value) (Result.bind (WrittenParsing.parse Validation.Script source) (fun unit -> W.checkSourceUnitsWithBase None false false [unit])) in
 let seed = Array.fold_left (fun seed value -> Int64.mul (Int64.logxor seed (Int64.of_int value)) 1099511628211L) (-3750763034362895579L) (HostText.utf16Units source) in
 let bits = ref seed in
 let values = List.init (if source = "" then 10016 else 64) (fun _ ->
  bits := Int64.logxor !bits (Int64.shift_left !bits 13);
  bits := Int64.logxor !bits (Int64.shift_right_logical !bits 7);
  bits := Int64.logxor !bits (Int64.shift_left !bits 17);
  Int64.float_of_bits !bits) in
 let values = [0.; -0.; nan; infinity; neg_infinity; 1.; 1e-5; 1e16; 1.2345678901234567; 1234567890.5; 2.2250738585072014e-308; Int64.float_of_bits 1L] @ values in
 tuple [list (result program) (C.observationFixtures source); sourceProgram source;
 list (fun value -> str (HostFloat.structural value)) values;
 if source = "" then list sourceProgram ["let recur (x: Int64) : Int64 = if x == 0 then x else recur (x - 1)
recur 1"; "let f (x: 'a) : 'a = (fun (y: 'a) -> y) x
f 1"] else `List []]

let observe = observeWith InstrumentedCheckedStructuralFormat.expr
let observeDisplay = observeWith InstrumentedCheckedStructuralFormat.toString
