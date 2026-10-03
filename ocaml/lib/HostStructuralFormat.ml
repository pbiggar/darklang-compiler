(* HostStructuralFormat.ml - Typed structural formatting and breakable block layout.
   Layout fitting follows FSharp.Core's sformat.fs implementation, blob
   e97978f46154b95fa87612bc655b54189cbd441d. Copyright (c) Microsoft Corporation.
   MIT license: ocaml/THIRD_PARTY_NOTICES.md. Reflection is replaced by typed
   values; fitting, precedence, limits, UTF-16 widths, and raw quotes are retained. *)
type value = Scalar of string | Text of string | Union of string * value list | Sequence of value list | Tuple of value list | Record of (string * value) list
(* A joint is unbreakable, breakable, or already broken at its indentation. *)
type joint = Unbreakable | Breakable of int | Broken of int
(* Either juxtaposition flag suppresses a space between neighboring leaves. *)
type layout = Leaf of bool * string * bool | Node of layout * layout * joint
let rec juxtLeft = function Leaf (left, _, _) -> left | Node (left, _, _) -> juxtLeft left
let rec juxtRight = function Leaf (_, _, right) -> right | Node (_, right, _) -> juxtRight right
let middle left right = juxtRight left || juxtLeft right
let empty = Leaf (true, "", true)
let isEmpty = function Leaf (true, "", true) -> true | Leaf _ | Node _ -> false
let node left right joint = if isEmpty left then right else if isEmpty right then left else Node (left, right, joint)
let word text = Leaf (false, text, false)
let left text = Leaf (false, text, true)
let right text = Leaf (true, text, false)
let join left right = node left right Unbreakable
let break indent left right = node left right (Breakable indent)
let bracket opening closing value = join (join (left opening) value) (right closing)
let separated separator = function [] -> empty | first :: rest -> List.fold_left (fun acc value -> break 0 (join acc (right separator)) value) first rest
(* Mutable break savings are threaded linearly through the fitting traversal. *)
type breaks = {mutable next : int; mutable outer : int; mutable stack : int array}
let pushBreak saving breaks =
 if breaks.next = Array.length breaks.stack then breaks.stack <- Array.init (breaks.next + 400) (fun index -> if index < breaks.next then breaks.stack.(index) else 0);
 breaks.stack.(breaks.next) <- saving; breaks.next <- breaks.next + 1
let popBreak breaks =
 if breaks.next = 0 then Crash.crash "popBreak: underflow";
 let broken = breaks.stack.(breaks.next - 1) < 0 in
 if breaks.outer = breaks.next then breaks.outer <- breaks.outer - 1;
 breaks.next <- breaks.next - 1; broken
let forceBreak breaks = if breaks.outer = breaks.next then None else
 let saving = breaks.stack.(breaks.outer) in breaks.stack.(breaks.outer) <- -saving; breaks.outer <- breaks.outer + 1; Some saving
let squash width layout =
 let breaks = {next = 0; outer = 0; stack = Array.make 400 0} in
 let rec fit pos = function
 | Leaf (_, text, _) as layout ->
   let textWidth = Array.length (HostText.utf16Units text) in
   let rec fitLeaf pos = if pos + textWidth <= width then layout, pos + textWidth, textWidth else match forceBreak breaks with None -> layout, pos + textWidth, textWidth | Some saving -> fitLeaf (pos - saving) in fitLeaf pos
 | Node (left, right, joint) ->
   let mid = if middle left right then 0 else 1 in
   let left, pos, offsetLeft = fit pos left in
   match joint with
   | Unbreakable -> let right, pos, offsetRight = fit (pos + mid) right in Node (left, right, Unbreakable), pos, offsetLeft + mid + offsetRight
   | Broken indent -> let right, pos, offsetRight = fit (pos - offsetLeft + indent) right in Node (left, right, Broken indent), pos, indent + offsetRight
   | Breakable indent ->
     let saving = offsetLeft + mid - indent in
     if saving > 0 then (
       pushBreak saving breaks;
       let right, pos, offsetRight = fit (pos + mid) right in
       if popBreak breaks then Node (left, right, Broken indent), pos, indent + offsetRight
       else Node (left, right, Breakable indent), pos, offsetLeft + mid + offsetRight)
     else let right, pos, offsetRight = fit (pos + mid) right in Node (left, right, Breakable indent), pos, offsetLeft + mid + offsetRight in
 let layout, _, _ = fit 0 layout in layout
let show layout =
 let output = Buffer.create 128 and column = ref 0 in
 let add text = Buffer.add_string output text; column := !column + Array.length (HostText.utf16Units text) in
 let rec visit indent = function
 | Leaf (_, text, _) -> add text
 | Node (left, right, Broken offset) -> visit indent left; Buffer.add_char output '\n'; Buffer.add_string output (String.make (indent + offset) ' '); column := indent + offset; visit (indent + offset) right
 | Node (left, right, Unbreakable) | Node (left, right, Breakable _) ->
   let juxtaposed = middle left right in visit indent left; if not juxtaposed then add " "; visit !column right in
 visit 0 layout; Buffer.contents output
let format value =
 let size = ref 10000 in
 let count () = if !size > 0 then decr size in
 let rec valueLayout depth precedence value =
   if depth <= 0 || !size <= 0 then word "..." else
   let depth = depth - 1 in
   match value with
   | Scalar value -> count (); word value
   | Text value -> count (); word ("\"" ^ value ^ "\"")
   | Union (name, fields) ->
     count ();
     (match fields with [] -> word name | _ :: _ ->
       let fields = match fields with [value] -> valueLayout depth 2 value
         | values -> bracket "(" ")" (separated "," (List.map (valueLayout depth 3) values)) in
       let basic = break 2 (word name) fields in if precedence <= 2 then bracket "(" ")" basic else basic)
   | Tuple values ->
     let basic = separated "," (List.map (valueLayout depth 3) values) in if precedence <= 3 then bracket "(" ")" basic else basic
   | Sequence [] -> count (); word "[]"
   | Sequence (first :: rest) ->
     let first = valueLayout depth 3 first in
     let rec consume remaining = function
       | _ when !size <= 0 -> [word "..."]
       | [] -> []
       | _ when remaining <= 0 -> [word "..."]
       | item :: rest -> let item = valueLayout depth 3 item in item :: consume (remaining - 1) rest in
     bracket "[" "]" (separated ";" (first :: consume 99 rest))
   | Record fields ->
     let fields = List.map (fun (name, value) -> count (); let value = valueLayout depth 3 value in break 1 (join (word name) (word "=")) value) fields in
     let body = match fields with [] -> empty | first :: rest -> List.fold_left (fun acc value -> node acc value (Broken 0)) first rest in
     join (join (word "{") body) (word "}") in
 valueLayout 100 3 value |> squash 80 |> show
let rec semanticValue = function
 | AST.TInt8 -> Union ("TInt8", []) | AST.TInt16 -> Union ("TInt16", []) | AST.TInt32 -> Union ("TInt32", []) | AST.TInt64 -> Union ("TInt64", [])
 | AST.TInt128 -> Union ("TInt128", []) | AST.TInt -> Union ("TInt", []) | AST.TUInt8 -> Union ("TUInt8", []) | AST.TUInt16 -> Union ("TUInt16", [])
 | AST.TUInt32 -> Union ("TUInt32", []) | AST.TUInt64 -> Union ("TUInt64", []) | AST.TUInt128 -> Union ("TUInt128", []) | AST.TBool -> Union ("TBool", [])
 | AST.TFloat64 -> Union ("TFloat64", []) | AST.TString -> Union ("TString", []) | AST.TBlob -> Union ("TBlob", []) | AST.TChar -> Union ("TChar", [])
 | AST.TDateTime -> Union ("TDateTime", []) | AST.TUnit -> Union ("TUnit", []) | AST.TNever -> Union ("TNever", []) | AST.TInternalRawPtr -> Union ("TInternalRawPtr", [])
 | AST.TFunction (args, result) -> Union ("TFunction", [Sequence (List.map semanticValue args); semanticValue result])
 | AST.TTuple args -> Union ("TTuple", [Sequence (List.map semanticValue args)])
 | AST.TRecord (name, args) -> Union ("TRecord", [Text name; Sequence (List.map semanticValue args)])
 | AST.TSum (name, args) -> Union ("TSum", [Text name; Sequence (List.map semanticValue args)])
 | AST.TList inner -> Union ("TList", [semanticValue inner]) | AST.TStream inner -> Union ("TStream", [semanticValue inner])
 | AST.TVar name -> Union ("TVar", [Text name]) | AST.TInferenceVar (display, identity) -> Union ("TInferenceVar", [Text display; Text identity])
 | AST.TDict (key, value) -> Union ("TDict", [semanticValue key; semanticValue value])
let semanticType value = format (semanticValue value)
let binOp = function
 | AST.Add -> "Add" | AST.Sub -> "Sub" | AST.Mul -> "Mul" | AST.Div -> "Div" | AST.Mod -> "Mod" | AST.Pow -> "Pow" | AST.Shl -> "Shl" | AST.Shr -> "Shr"
 | AST.BitAnd -> "BitAnd" | AST.BitOr -> "BitOr" | AST.BitXor -> "BitXor" | AST.StringConcat -> "StringConcat" | AST.Eq -> "Eq" | AST.Neq -> "Neq"
 | AST.Lt -> "Lt" | AST.Gt -> "Gt" | AST.Lte -> "Lte" | AST.Gte -> "Gte" | AST.And -> "And" | AST.Or -> "Or"
