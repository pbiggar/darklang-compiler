(* StructuralFormat.ml - Bounded diagnostic layouts using OCaml Format boxes. *)
type value = StructuralValue.value =
  | Scalar of string
  | Text of string
  | Union of string * value list
  | Sequence of value list
  | Array of value list
  | Tuple of value list
  | Record of (string * value) list

let format value =
  let buffer = Buffer.create 128 in
  let formatter = Format.formatter_of_buffer buffer in
  Format.pp_set_margin formatter 80;
  let remaining = ref 10000 in
  let rec print depth precedence formatter value =
    if depth = 0 || !remaining = 0 then Format.pp_print_string formatter "..."
    else (
      decr remaining;
      let child = print (depth - 1) in
      let separated separator precedence formatter values =
        let rec items count = function
          | [] -> ()
          | _ when count = 100 || !remaining = 0 ->
              Format.pp_print_string formatter "..."
          | value :: rest ->
              child precedence formatter value;
              if rest <> [] then (
                Format.pp_print_string formatter separator;
                Format.pp_print_space formatter ();
                items (count + 1) rest)
        in
        items 0 values
      in
      match value with
      | Scalar text -> Format.pp_print_string formatter text
      | Text text -> Format.fprintf formatter "%S" text
      | Union (name, []) -> Format.pp_print_string formatter name
      | Union (name, [ value ]) ->
          if precedence <= 2 then Format.pp_print_char formatter '(';
          Format.fprintf formatter "@[%s@ %a@]" name (child 2) value;
          if precedence <= 2 then Format.pp_print_char formatter ')'
      | Union (name, values) ->
          if precedence <= 2 then Format.pp_print_char formatter '(';
          Format.fprintf formatter "@[%s@ (@[%a@])@]" name (separated "," 3)
            values;
          if precedence <= 2 then Format.pp_print_char formatter ')'
      | Tuple values ->
          if precedence <= 3 then Format.pp_print_char formatter '(';
          Format.fprintf formatter "@[%a@]" (separated "," 3) values;
          if precedence <= 3 then Format.pp_print_char formatter ')'
      | Sequence values ->
          Format.fprintf formatter "[@[%a@]]" (separated ";" 3) values
      | Array values ->
          Format.fprintf formatter "[|@[%a@]|]" (separated ";" 3) values
      | Record fields ->
          let field formatter (name, value) =
            Format.fprintf formatter "@[%s =@ %a@]" name (child 3) value
          in
          Format.fprintf formatter "{@[<hov 1>%a@]}"
            (Format.pp_print_list
               ~pp_sep:(fun formatter () -> Format.fprintf formatter ";@ ")
               field)
            fields)
  in
  print 100 3 formatter value;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer

let rec semanticValue = function
  | AST.TInt8 -> Union ("TInt8", [])
  | AST.TInt16 -> Union ("TInt16", [])
  | AST.TInt32 -> Union ("TInt32", [])
  | AST.TInt64 -> Union ("TInt64", [])
  | AST.TInt128 -> Union ("TInt128", [])
  | AST.TInt -> Union ("TInt", [])
  | AST.TUInt8 -> Union ("TUInt8", [])
  | AST.TUInt16 -> Union ("TUInt16", [])
  | AST.TUInt32 -> Union ("TUInt32", [])
  | AST.TUInt64 -> Union ("TUInt64", [])
  | AST.TUInt128 -> Union ("TUInt128", [])
  | AST.TBool -> Union ("TBool", [])
  | AST.TFloat64 -> Union ("TFloat64", [])
  | AST.TString -> Union ("TString", [])
  | AST.TBlob -> Union ("TBlob", [])
  | AST.TChar -> Union ("TChar", [])
  | AST.TDateTime -> Union ("TDateTime", [])
  | AST.TUnit -> Union ("TUnit", [])
  | AST.TNever -> Union ("TNever", [])
  | AST.TInternalRawPtr -> Union ("TInternalRawPtr", [])
  | AST.TFunction (args, result) ->
      Union
        ( "TFunction",
          [ Sequence (List.map semanticValue args); semanticValue result ] )
  | AST.TTuple args ->
      Union ("TTuple", [ Sequence (List.map semanticValue args) ])
  | AST.TRecord (name, args) ->
      Union ("TRecord", [ Text name; Sequence (List.map semanticValue args) ])
  | AST.TSum (name, args) ->
      Union ("TSum", [ Text name; Sequence (List.map semanticValue args) ])
  | AST.TList inner -> Union ("TList", [ semanticValue inner ])
  | AST.TStream inner -> Union ("TStream", [ semanticValue inner ])
  | AST.TVar name -> Union ("TVar", [ Text name ])
  | AST.TInferenceVar (display, identity) ->
      Union ("TInferenceVar", [ Text display; Text identity ])
  | AST.TDict (key, value) ->
      Union ("TDict", [ semanticValue key; semanticValue value ])

let semanticType value = format (semanticValue value)

let binOp = function
  | AST.Add -> "Add"
  | AST.Sub -> "Sub"
  | AST.Mul -> "Mul"
  | AST.Div -> "Div"
  | AST.Mod -> "Mod"
  | AST.Pow -> "Pow"
  | AST.Shl -> "Shl"
  | AST.Shr -> "Shr"
  | AST.BitAnd -> "BitAnd"
  | AST.BitOr -> "BitOr"
  | AST.BitXor -> "BitXor"
  | AST.StringConcat -> "StringConcat"
  | AST.Eq -> "Eq"
  | AST.Neq -> "Neq"
  | AST.Lt -> "Lt"
  | AST.Gt -> "Gt"
  | AST.Lte -> "Lte"
  | AST.Gte -> "Gte"
  | AST.And -> "And"
  | AST.Or -> "Or"
