// ListDisplay.fs - Shared list display helper lookup
//
// Centralizes the mapping from list element types to the stdlib display
// functions that render list values for printing.

module ListDisplay

let getDisplayStringFunc (elemType: AST.Type) : string option =
    match elemType with
    | AST.TInt64 -> Some "Stdlib.List.toDisplayString_i64"
    | AST.TInt -> Some "Stdlib.List.toDisplayString_int"
    | AST.TBool -> Some "Stdlib.List.toDisplayString_bool"
    | AST.TString -> Some "Stdlib.List.toDisplayString_str"
    | AST.TChar -> Some "Stdlib.List.toDisplayString_char"
    | AST.TFloat64 -> Some "Stdlib.List.toDisplayString_f64"
    | AST.TList AST.TInt64 -> Some "Stdlib.List.toDisplayString_list_i64"
    | AST.TInt8
    | AST.TInt16
    | AST.TInt32
    | AST.TInt128
    | AST.TUInt8
    | AST.TUInt16
    | AST.TUInt32
    | AST.TUInt64
    | AST.TUInt128
    | AST.TBytes
    | AST.TUnit
    | AST.TRuntimeError
    | AST.TFunction _
    | AST.TTuple _
    | AST.TRecord _
    | AST.TSum _
    | AST.TList _
    | AST.TVar _
    | AST.TRawPtr
    | AST.TDict _ -> None
