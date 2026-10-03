[@@@warning "-4"]
open Dark_compiler
module P = InstrumentedLoweringPrimitives
module A = InstrumentedANF
module C = InstrumentedCheckedAST
module M = StringOrder.Map
let tuple values = `Assoc ["tuple", `List values]
let list encode values = `List (List.map encode values)
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let option encode = function None -> SemanticJson.union "FSharpOption" "None" [] | Some value -> SemanticJson.union "FSharpOption" "Some" [encode value]
let result encode = function Ok value -> SemanticJson.union "FSharpResult" "Ok" [encode value] | Error error -> SemanticJson.union "FSharpResult" "Error" [str error]
let map encode values = `Assoc ["map", list (fun (name, value) -> tuple [str name; encode value]) (M.bindings values)]
let case (value : P.sumCase) = SemanticJson.record "SumCase" ["TypeParams", list str value.P.typeParams; "Tag", SemanticJson.int32 value.P.tag; "Fields", list typ value.P.fields]
let index = map (map case)
let metadata (value : P.sumMetadata) = SemanticJson.record "SumMetadata" ["Names", `Assoc ["set", list str (StringOrder.Set.elements value.P.names)]; "Cases", index value.P.cases]
let observe source =
 let calls = ref [] in
 let resolve name = calls := name :: !calls; AST.functionId 9L in
 let capture encode work =
  calls := [];
  try let value = work () in tuple [list str (List.rev !calls); result encode (Ok value)]
  with Failure message -> tuple [list str (List.rev !calls); result encode (Error message)] in
 let types = [AST.TInt8; AST.TInt16; AST.TInt32; AST.TInt64; AST.TInt128; AST.TInt; AST.TUInt8; AST.TUInt16; AST.TUInt32; AST.TUInt64; AST.TUInt128; AST.TBool; AST.TFloat64; AST.TString; AST.TBlob; AST.TChar; AST.TDateTime; AST.TUnit; AST.TNever; AST.TInternalRawPtr; AST.TVar source; AST.TInferenceVar (source,"fixed"); AST.TRecord ("R", []); AST.TSum ("S", []); AST.TTuple [AST.TInt64; AST.TString]; AST.TList AST.TString; AST.TStream AST.TString; AST.TDict (AST.TString, AST.TInt64); AST.TFunction ([AST.TInt64], AST.TString)] in
 let variants = M.of_list ["One.C", ("One", ["a"], 5, [AST.TVar "a"]); "Null.A", ("Null", ["a"], 2, []); "Null.Z", ("Null", ["a"], 7, [AST.TVar "a"]); "Bad.B", ("Bad", [], 3, [AST.TString; AST.TInt64]); "plain", ("Lost", [], 4, [])] in
 let sums = P.sumRepresentationIndex variants in
 let more = M.of_list ["One.D", ("One", [], 8, []); "Null.Z", ("Null", [], 0, [AST.TBool])] in
 let args = [ []; [A.UnitLiteral]; [A.StringLiteral source]; [A.StringLiteral source; A.IntLiteral (A.Int64 (-1L))]; [A.StringLiteral source; A.IntLiteral (A.Int64 0L); A.UnitLiteral]; [A.UnitLiteral; A.UnitLiteral]; [A.StringLiteral source; A.UnitLiteral; A.UnitLiteral; A.UnitLiteral]] in
 let names = ["Builtin.crash"; "Builtin.print"; "Builtin.printLine"; "Builtin.stdinReadLine"; "Builtin.testRuntimeError"; "Builtin.unwrap"; "Darklang.Stdlib.Bool.not"; "Darklang.Stdlib.Cli.__argv"; "Darklang.Stdlib.Cli.__cpuCount"; "Darklang.Stdlib.Cli.__createExclusive"; "Darklang.Stdlib.Cli.__environmentPacked"; "Darklang.Stdlib.Cli.__execute"; "Darklang.Stdlib.Cli.__getenv"; "Darklang.Stdlib.Cli.__getpid"; "Darklang.Stdlib.Cli.__getuid"; "Darklang.Stdlib.Cli.__hostArchitectureCode"; "Darklang.Stdlib.Cli.__hostOSCode"; "Darklang.Stdlib.Cli.__hostname"; "Darklang.Stdlib.Cli.__kill"; "Darklang.Stdlib.Cli.__processIO"; "Darklang.Stdlib.Cli.__runProcess"; "Darklang.Stdlib.Cli.__setenv"; "Darklang.Stdlib.Cli.__sleep"; "Darklang.Stdlib.Cli.__spawnProcess"; "Darklang.Stdlib.Cli.__terminateProcess"; "Darklang.Stdlib.Cli.__unsetenv"; "Darklang.Stdlib.Crypto.__secureRandomFill"; "Darklang.Stdlib.DateTime.__fromUnixTimeTicks"; "Darklang.Stdlib.DateTime.__now"; "Darklang.Stdlib.DateTime.__toUnixTimeTicks"; "Darklang.Stdlib.File.appendText"; "Darklang.Stdlib.File.createDirectory"; "Darklang.Stdlib.File.currentDirectory"; "Darklang.Stdlib.File.delete"; "Darklang.Stdlib.File.exists"; "Darklang.Stdlib.File.isDirectory"; "Darklang.Stdlib.File.listDirectoryPacked"; "Darklang.Stdlib.File.readBlob"; "Darklang.Stdlib.File.setExecutable"; "Darklang.Stdlib.File.writeBlob"; "Darklang.Stdlib.File.writeFromPtr"; "Darklang.Stdlib.Float.__toBits"; "Darklang.Stdlib.Float.__toInt64Unchecked"; "Darklang.Stdlib.Float.negate"; "Darklang.Stdlib.Float.sqrt"; "Darklang.Stdlib.Int.__equals"; "Darklang.Stdlib.Int.__randomInt64Word"; "Darklang.Stdlib.Int128.__equalsWords"; "Darklang.Stdlib.Int128.__fromInt"; "Darklang.Stdlib.Int128.__fromWords"; "Darklang.Stdlib.Int128.__toInt"; "Darklang.Stdlib.Int16"; "Darklang.Stdlib.Int32"; "Darklang.Stdlib.Int64"; "Darklang.Stdlib.Int64.toFloat"; "Darklang.Stdlib.Int8"; "Darklang.Stdlib.Network.__close"; "Darklang.Stdlib.Network.__connect4"; "Darklang.Stdlib.Network.__connect6"; "Darklang.Stdlib.Network.__receive"; "Darklang.Stdlib.Network.__receiveTimeout"; "Darklang.Stdlib.Network.__send"; "Darklang.Stdlib.Network.__sendTimeout"; "Darklang.Stdlib.Network.__tcp4Socket"; "Darklang.Stdlib.Network.__tcp6Socket"; "Darklang.Stdlib.Network.__udp4Socket"; "Darklang.Stdlib.Network.__udp6Socket"; "Darklang.Stdlib.UInt128.__equalsWords"; "Darklang.Stdlib.UInt128.__fromInt"; "Darklang.Stdlib.UInt128.__fromWords"; "Darklang.Stdlib.UInt128.__toInt"; "Darklang.Stdlib.UInt16"; "Darklang.Stdlib.UInt32"; "Darklang.Stdlib.UInt64"; "Darklang.Stdlib.UInt8"; "__blob_to_rawptr"; "__dark_internal_eq_helper_dispatch"; "__dict_get_tag"; "__dict_is_null"; "__dict_to_rawptr"; "__empty_dict"; "__int128_to_int"; "__int128_to_rawptr"; "__int64_to_int16"; "__int64_to_int32"; "__int64_to_int8"; "__int64_to_uint16"; "__int64_to_uint32"; "__int64_to_uint64_bits"; "__int64_to_uint8"; "__int_to_int128"; "__int_to_rawptr"; "__int_to_uint128"; "__int_to_word"; "__list_array_release_small"; "__list_empty"; "__list_get_tag"; "__list_is_null"; "__list_to_rawptr"; "__mapped_alloc"; "__mapped_free"; "__raw_alloc"; "__raw_free"; "__raw_get"; "__raw_get_byte"; "__raw_slot_init"; "__raw_slot_init requires a concrete slot type"; "__raw_take"; "__raw_write_byte"; "__raw_write_word"; "__rawptr_to_blob"; "__rawptr_to_dict"; "__rawptr_to_int"; "__rawptr_to_int128"; "__rawptr_to_list"; "__rawptr_to_stream"; "__rawptr_to_string"; "__rawptr_to_uint128"; "__refcount_dec_string"; "__refcount_inc_string"; "__stream_to_rawptr"; "__string_concat_raw"; "__string_to_rawptr"; "__uint128_to_int"; "__uint128_to_rawptr"; "__uint16_to_int64"; "__uint32_to_int64"; "__uint64_to_int64_bits"; "__uint8_to_int64"; "__word_to_int"; "missing"; "Darklang.Stdlib.Int64.shiftLeft"; "Darklang.Stdlib.UInt64.bitwiseNot"; "__raw_get_byte_i64"; "__raw_get_str"; "__raw_take_str"; "__raw_slot_init_str"; "__raw_slot_init_fn_i64_to_str"; "__rawptr_to_dict_str_i64"; "__rawptr_to_list_str"; "__rawptr_to_stream_str"; "__raw_get_bad_type"] in
 let intrinsic name args = capture (list (option SemanticANF.aNF_cExpr)) (fun () ->
  let file = P.tryFileIntrinsic name args in let cli = P.tryCliIntrinsic name args in let presentation = P.tryPresentationIntrinsic name args in
  let float = P.tryFloatIntrinsic name args in let canonical = P.tryCanonicalPrimitiveIntrinsic name args in
  let raw = P.tryRawMemoryIntrinsic resolve (StringOrder.Set.singleton "S") name args in let random = P.tryRandomIntrinsic name args in let date = P.tryDateTimeIntrinsic name args in
  [file; cli; presentation; float; canonical; raw; random; date]) in
 let sourceToken = if Array.length (HostText.utf16Units source) <= 64 && List.length (String.split_on_char '_' source) <= 5 then source else "source" in
 let mangled = sourceToken :: [""; "i64"; "runtime_error"; "rawptr"; "R"; "S"; "S_i64"; "R_i64_str"; "a"; "λ"; "𐐨"; "\xE1\xB2\x8A"; "tup"; "tup0"; "tup2_i64_str"; "tup_i64_str"; "tup3_i64"; "fn_i64_to_str"; "fn_to_str"; "fn_i64_to_fn_str_to_bool"; "dict_str_list_i64"; "stream_R_i64"; "a$b"; "R__i64"; "tup2147483648_i64"; "tup+1_i64"] in
 let integers = [Z.neg (Z.shift_left Z.one 127); Z.of_int (-1); Z.zero; Z.one; Z.pred (Z.shift_left Z.one 127); Z.pred (Z.shift_left Z.one 128)] in
 let patterns = [C.PInt64 (-1L); C.PInt8Literal (-128); C.PInt16Literal (-32768); C.PInt32Literal Int32.min_int; C.PUInt8Literal 255; C.PUInt16Literal 65535; C.PUInt32Literal 4294967295L; C.PUInt64Literal (-1L); C.PWildcard; C.PBool true; C.PString source] in
 let expressions = [C.UnitLiteral; C.Int64Literal (-1L); C.Int128Literal (Z.neg (Z.shift_left Z.one 127)); C.UInt128Literal (Z.pred (Z.shift_left Z.one 128)); C.Int8Literal (-128); C.Int16Literal (-32768); C.Int32Literal Int32.min_int; C.UInt8Literal 255; C.UInt16Literal 65535; C.UInt32Literal 4294967295L; C.UInt64Literal (-1L); C.BoolLiteral true; C.BoolLiteral false; C.FloatLiteral (-0.); C.FloatLiteral infinity; C.StringLiteral source; C.CharLiteral source; C.RuntimeError source] in
 let constructor,symbols = C.internConstructor "Null" "Z" 7 (C.emptySymbols ()) in
 let owner = AST.constructorIdOwner constructor in let names = names @ [source] in
 let reference = {C.typeId = owner; constructorId = constructor; typeArgs = []} in
 let typeNames = C.semanticMetadata symbols in
 tuple [
 str P.eqHelperDispatchMarker;
 list (fun value -> tuple [typ value; str (P.typeToString value); option SemanticANF.memoryModel_canonicalBufferKind (P.canonicalBufferKindForType value); `Bool (P.canUseTransparentPayload value); `Bool (P.canUseNullaryZeroForPayload value)]) types;
 index sums; metadata (P.mergeSumMetadata (P.sumMetadataFromVariantLookup variants) (P.sumMetadataFromVariantLookup more));
 list (fun value -> list (fun owner -> capture (fun (transparent, nullable, spare, payload) -> tuple [option typ transparent; option typ nullable; option (fun value -> `Assoc ["kind", `String "int64"; "value", `String (Int64.to_string value)]) spare; SemanticANF.aNF_cExpr payload]) (fun () -> P.transparentSumPayloadType owner [value] sums, P.nullablePointerSumPayloadType owner [value] sums, P.spareImmediateSumSentinel owner [value] sums, P.sumPayloadExpr (AST.TSum (owner, [value])) (A.StringLiteral source) sums)) ["One"; "Null"; "Bad"; "Missing"]) types;
 list (fun name -> tuple [str name; list (intrinsic name) args; `Bool (P.isBuiltinUnwrapName name); `Bool (P.isRuntimeFailureName name); `Bool (P.isSourceCrashName name); `Bool (P.isBuiltinTestRuntimeErrorName name)]) names;
 list (fun value -> tuple [str value; result typ (P.tryParseMangledType variants value)]) mangled;
 list (fun value -> capture (list SemanticANF.aNF_cExpr) (fun () ->
  let a = P.int128Construction resolve value in let b = P.uint128Construction resolve value in
  let c = P.int128LiteralComparison resolve (A.StringLiteral source) value in let d = P.uint128LiteralComparison resolve (A.StringLiteral source) value in [a; b; c; d])) integers;
 list (fun value -> option SemanticANF.aNF_sizedInt (P.patternLiteralToSizedInt value)) patterns;
 list (fun value -> option str (P.unwrapErrorPayloadToString value)) expressions;
 list (fun args -> list (fun value -> capture C.observationExpr (fun () -> P.materializeComparisonPlan resolve value args)) types) [ []; [C.StringLiteral source]; [C.StringLiteral source; C.UnitLiteral] ];
 list (fun args -> capture C.observationExpr (fun () -> P.materializeFunctionComparisonPlan (AST.bindingId 0) (AST.bindingId 1) args)) [ []; [C.UnitLiteral]; [C.StringLiteral source; C.UnitLiteral] ];
 tuple [option str (P.tryFindRecordTypeNameById owner typeNames); option str (P.tryFindSumTypeNameById owner typeNames);
 option (fun (owner, parameters, tag, fields) -> tuple [str owner; list str parameters; SemanticJson.int32 tag; list typ fields]) (P.tryFindVariantForType "Z" (AST.TSum ("Null", [])) variants);
 option (fun (owner, parameters, tag, fields) -> tuple [str owner; list str parameters; SemanticJson.int32 tag; list typ fields]) (P.tryFindVariantByTag "Null" 7 sums);
 option (fun (owner, parameters, tag, fields) -> tuple [str owner; list str parameters; SemanticJson.int32 tag; list typ fields]) (P.tryFindVariantByConstructorId owner "Null" constructor variants);
 option (fun (owner, parameters, tag, fields) -> tuple [str owner; list str parameters; SemanticJson.int32 tag; list typ fields]) (P.tryFindVariantForTypeById constructor (AST.TSum ("Null", [])) typeNames variants);
 `Bool (P.constructorReferenceMatches "Null" "Null.Z" reference typeNames variants)]]
