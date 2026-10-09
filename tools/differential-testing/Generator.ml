(* Generator.ml - Typed, bounded programs formatted by the compiler's canonical printer. *)
open Dark_compiler
open! AST

let choose random = function
  | [] -> Crash.crash "Differential test generator choice set was empty"
  | items -> (
      match
        List.nth_opt items (Random.State.int random (List.length items))
      with
      | Some item -> item
      | None -> Crash.crash "Differential test generator choice index was outside its set")

let scalarTypes =
  [
    TInt8;
    TInt16;
    TInt32;
    TInt64;
    TInt128;
    TInt;
    TUInt8;
    TUInt16;
    TUInt32;
    TUInt64;
    TUInt128;
    TBool;
    TFloat64;
    TString;
    TChar;
    TUnit;
  ]

let supportedTypes =
  scalarTypes
  @ List.map (fun typ -> TList typ) scalarTypes
  @ List.map (fun typ -> TDict (TString, typ)) scalarTypes
  @ [
      TTuple [ TInt64; TBool ];
      TRecord ("TestBox", []);
      TBlob;
      TDateTime;
      TStream TInt64;
    ]

let nonempty head tail = { NonEmptyList.head; tail }

let call name arguments =
  match arguments with
  | [] -> Crash.crash "Differential test generator emitted a call without arguments"
  | head :: tail -> Apply (Var name, [], nonempty head tail)

let field = unresolvedRecordFieldReference

let box value flag =
  RecordLiteral
    ( unresolvedRecordReference "TestBox" [],
      [ (field "value", value); (field "flag", flag) ] )

let rec literal random typ =
  let signed () = Random.State.int random 201 - 100 in
  let unsigned () = Random.State.int random 201 in
  match typ with
  | TInt8 -> Int8Literal (signed ())
  | TInt16 -> Int16Literal (signed ())
  | TInt32 -> Int32Literal (Int32.of_int (signed ()))
  | TInt64 -> Int64Literal (Int64.of_int (signed ()))
  | TInt128 -> Int128Literal (Z.of_int (signed ()))
  | TInt -> BigIntLiteral (Z.of_int (signed ()))
  | TUInt8 -> UInt8Literal (unsigned ())
  | TUInt16 -> UInt16Literal (unsigned ())
  | TUInt32 -> UInt32Literal (Int64.of_int (unsigned ()))
  | TUInt64 -> UInt64Literal (Int64.of_int (unsigned ()))
  | TUInt128 -> UInt128Literal (Z.of_int (unsigned ()))
  | TBool -> BoolLiteral (Random.State.bool random)
  | TFloat64 -> FloatLiteral (float_of_int (signed ()) /. 4.)
  | TString ->
      StringLiteral
        (choose random
           [ ""; "a"; "dark"; "line\nbreak"; "quote\"slash\\"; "héllo" ])
  | TChar -> CharLiteral (choose random [ "a"; "é"; "🚀" ])
  | TUnit -> UnitLiteral
  | TTuple [ TInt64; TBool ] ->
      TupleLiteral [ literal random TInt64; literal random TBool ]
  | TList typ ->
      ListLiteral
        (List.init
           (Random.State.int random 3 + 1)
           (fun _ -> literal random typ))
  | TDict (TString, typ) ->
      DictLiteral
        ( TString,
          typ,
          [
            (StringLiteral "a", literal random typ);
            (StringLiteral "b", literal random typ);
          ] )
  | TRecord ("TestBox", []) ->
      box (literal random TInt64) (literal random TBool)
  | TBlob -> call "Stdlib.Blob.fromString" [ literal random TString ]
  | TDateTime -> call "Stdlib.DateTime.fromMilliseconds" [ literal random TInt ]
  | TStream TInt64 ->
      call "Stdlib.Stream.fromList" [ literal random (TList TInt64) ]
  | _ -> Crash.crash "Unsupported generated literal type"

let matched ?guard pattern body =
  { patterns = NonEmptyList.singleton pattern; guard; body }

let generate random depth =
  let next = ref 0 in
  let fresh () =
    let name = "test" ^ string_of_int !next in
    incr next;
    name
  in
  let leaf environment typ =
    let names =
      List.filter_map
        (fun (name, candidate) -> if candidate = typ then Some name else None)
        environment
    in
    match names with
    | [] -> literal random typ
    | _ ->
        if Random.State.int random 3 = 0 then literal random typ
        else Var (choose random names)
  in
  let rec expression environment depth typ =
    if depth <= 0 then leaf environment typ
    else
      let child typ = expression environment (depth - 1) typ in
      let operation () =
        match typ with
        | TInt64 ->
            BinOp (choose random [ Add; Sub; Mul ], child TInt64, child TInt64)
        | TBool ->
            if Random.State.int random 3 = 0 then
              BinOp (choose random [ And; Or ], child TBool, child TBool)
            else
              let operand =
                choose random (List.filter (( <> ) TUnit) scalarTypes)
              in
              let op =
                choose random
                  (if operand = TInt64 then [ Eq; Neq; Lt; Gt; Lte; Gte ]
                   else [ Eq; Neq ])
              in
              BinOp (op, child operand, child operand)
        | TString -> BinOp (StringConcat, child TString, child TString)
        | TTuple [ TInt64; TBool ] -> TupleLiteral [ child TInt64; child TBool ]
        | TList element -> ListLiteral [ child element ]
        | TDict (TString, value) ->
            DictLiteral (TString, value, [ (StringLiteral "a", child value) ])
        | TRecord ("TestBox", []) -> box (child TInt64) (child TBool)
        | _ -> leaf environment typ
      in
      let feature () =
        match (Random.State.int random 20, typ) with
        | 0, TInt64 ->
            let name = fresh () in
            Let
              ( LPTuple (LPVariable name, LPWildcard, []),
                TupleLiteral [ child TInt64; child TBool ],
                Var name )
        | 1, TBool ->
            let name = fresh () in
            Let
              ( LPTuple (LPWildcard, LPVariable name, []),
                TupleLiteral [ child TInt64; child TBool ],
                Var name )
        | 2, TInt64 ->
            Match
              ( ListLiteral [ child TInt64 ],
                [
                  matched (PList [ PVar "item" ])
                    (BinOp (Add, Var "item", child TInt64));
                  matched PWildcard (Int64Literal 0L);
                ] )
        | 3, _ ->
            Apply
              ( Lambda
                  ( NonEmptyList.singleton (typedLambdaVariable "input" typ),
                    Some typ,
                    Var "input" ),
                [],
                NonEmptyList.singleton (child typ) )
        | 4, TBool ->
            let value =
              InterpolatedString
                [ StringText "prefix:"; StringExpr (child TString) ]
            in
            BinOp (Eq, value, value)
        | 5, TInt64 ->
            Match
              ( Constructor
                  (UnresolvedConstructor None, "Some", [ child TInt64 ]),
                [
                  matched
                    (PConstructor ("Some", [ PVar "item" ]))
                    (BinOp (Add, Var "item", child TInt64));
                  matched (PConstructor ("None", [])) (Int64Literal 0L);
                ] )
        | 6, TBool ->
            Match
              ( Constructor
                  (UnresolvedConstructor None, "Some", [ child TInt64 ]),
                [
                  matched
                    ~guard:(BinOp (Gt, Var "item", Int64Literal 0L))
                    (PConstructor ("Some", [ PVar "item" ]))
                    (BoolLiteral true);
                  matched PWildcard (BoolLiteral false);
                ] )
        | (7 | 8), (TInt64 | TBool) ->
            let name = fresh () in
            Let
              ( LPVariable name,
                box (child TInt64) (child TBool),
                RecordAccess
                  (Var name, field (if typ = TInt64 then "value" else "flag"))
              )
        | 9, TInt64 -> call "testIdentity" [ child TInt64 ]
        | 10, TInt64 ->
            Match
              ( Constructor (UnresolvedConstructor None, "Ok", [ child TInt64 ]),
                [
                  matched
                    (PConstructor ("Ok", [ PVar "result" ]))
                    (Var "result");
                  matched
                    (PConstructor ("Error", [ PWildcard ]))
                    (Int64Literal 0L);
                ] )
        | 11, TInt64 ->
            let name = fresh () in
            Let
              ( LPVariable name,
                box (child TInt64) (BoolLiteral true),
                RecordAccess
                  ( RecordUpdate (Var name, [ (field "value", child TInt64) ]),
                    field "value" ) )
        | 12, TInt64 ->
            Match
              ( ListLiteral [ child TInt64; child TInt64 ],
                [
                  matched (PListCons ([ PVar "head" ], PWildcard)) (Var "head");
                  matched (PList []) (Int64Literal 0L);
                ] )
        | 13, TBool ->
            let value = call "Stdlib.Blob.length" [ child TBlob ] in
            BinOp (Eq, value, value)
        | 14, TBool ->
            let value =
              call "Stdlib.DateTime.toMilliseconds" [ child TDateTime ]
            in
            BinOp (Eq, value, value)
        | 15, TInt64 ->
            let values =
              call "Stdlib.Stream.toList"
                [
                  call "Stdlib.Stream.fromList" [ ListLiteral [ child TInt64 ] ];
                ]
            in
            Match
              ( values,
                [
                  matched (PList [ PVar "item" ]) (Var "item");
                  matched PWildcard (Int64Literal 0L);
                ] )
        | 16, _ ->
            Apply
              (Var "testGeneric", [ typ ], NonEmptyList.singleton (child typ))
        | 17, _ ->
            let other = choose random scalarTypes in
            Apply
              ( Var "testSelect",
                [ typ; other ],
                nonempty (child typ) [ child other ] )
        | 18, TInt64 ->
            call "testRecur"
              [ Int64Literal (Int64.of_int (Random.State.int random 9)) ]
        | 19, TBool ->
            call
              (choose random [ "testEven"; "testOdd" ])
              [ Int64Literal (Int64.of_int (Random.State.int random 9)) ]
        | _ -> operation ()
      in
      match Random.State.int random 7 with
      | 0 -> leaf environment typ
      | 1 -> If (child TBool, child typ, child typ)
      | 2 ->
          let bindingType = choose random supportedTypes in
          let binding = child bindingType in
          let name = fresh () in
          Let
            ( LPVariable name,
              binding,
              expression ((name, bindingType) :: environment) (depth - 1) typ )
      | 3 | 4 -> operation ()
      | _ -> feature ()
  in
  let functionDef name typeParams parameters returnType body =
    match parameters with
    | [] -> Crash.crash "Generated function had no parameters"
    | head :: tail ->
        FunctionDef
          {
            name;
            typeParams;
            params = nonempty head tail;
            returnType;
            body;
            recursion = None;
          }
  in
  let count = Var "count"
  and zero = Int64Literal 0L
  and one = Int64Literal 1L in
  let mutual name callee base =
    functionDef name []
      [ ("count", TInt64) ]
      TBool
      (If
         ( BinOp (Lte, count, zero),
           BoolLiteral base,
           call callee [ BinOp (Sub, count, one) ] ))
  in
  let result = expression [] depth (choose random [ TInt64; TBool ]) in
  Program
    [
      TypeDef
        (RecordDef ("TestBox", [], [ ("value", TInt64); ("flag", TBool) ]));
      functionDef "testIdentity" [] [ ("input", TInt64) ] TInt64 (Var "input");
      functionDef "testGeneric" [ "t" ]
        [ ("input", TVar "t") ]
        (TVar "t") (Var "input");
      functionDef "testSelect" [ "a"; "b" ]
        [ ("selected", TVar "a"); ("other", TVar "b") ]
        (TVar "a") (Var "selected");
      functionDef "testRecur" []
        [ ("count", TInt64) ]
        TInt64
        (If
           ( BinOp (Lte, count, zero),
             zero,
             BinOp (Add, one, call "testRecur" [ BinOp (Sub, count, one) ]) ));
      mutual "testEven" "testOdd" true;
      mutual "testOdd" "testEven" false;
      Expression ([], result);
    ]
