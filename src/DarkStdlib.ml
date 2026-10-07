(*
   DarkStdlib.ml - Standard Library Module Definitions
   Defines intrinsic Stdlib module signatures used directly by the compiler.
   Non-intrinsic stdlib functions are loaded from stdlib/*.dark.
*)
(* DarkStdlib.ml - Standard Library Module Definitions. *)
open! AST
module M = StringOrder.Map
let fn name typeParams paramTypes returnType : AST.moduleFunc = {AST.name; typeParams; paramTypes; returnType}
let integerBitwiseFunctions typ = [
  fn "bitwiseAnd" [] [typ; typ] (typ);
  fn "bitwiseOr" [] [typ; typ] (typ);
  fn "bitwiseXor" [] [typ; typ] (typ);
  fn "shiftLeft" [] [typ; typ] (typ);
  fn "shiftRight" [] [typ; typ] (typ);
  fn "bitwiseNot" [] [typ] (typ);
]
let integerBitwiseModule name typ : AST.moduleDef = {AST.name = "Darklang.Stdlib." ^ name; functions = integerBitwiseFunctions typ}
(*
   Helper to create Result<T, String> type
*)
let resultType okType = TSum ("Darklang.Stdlib.Result.Result", [okType; TString])
let boolIntrinsicModule : AST.moduleDef = {AST.name = "Darklang.Stdlib.Bool"; functions = [
  fn "not" [] [TBool] (TBool);
]}
(*
   Intrinsic Stdlib.Int64 functions
   toFloat : (Int64) -> Float
*)
let int64IntrinsicModule : AST.moduleDef = {AST.name = "Darklang.Stdlib.Int64"; functions = integerBitwiseFunctions TInt64 @ [
  fn "toFloat" [] [TInt64] (TFloat64);
]}
(*
   Intrinsic Stdlib.Float functions
*)
let floatIntrinsicModule : AST.moduleDef = {AST.name = "Darklang.Stdlib.Float"; functions = [
  fn "sqrt" [] [TFloat64] (TFloat64);
  fn "negate" [] [TFloat64] (TFloat64);
  fn "__toBits" [] [TFloat64] (TUInt64);
  fn "__toInt64Unchecked" [] [TFloat64] (TInt64);
]}
(*
   Internal native operations supporting the public Stdlib.Cli modules.
   Portable policy stays in Dark; these typed effects are lowered by the compiler.
*)
let cliIntrinsicModule : AST.moduleDef = {AST.name = "Darklang.Stdlib.Cli"; functions = [
  fn "__execute" [] [TString] (TRecord ("Darklang.Stdlib.Cli.NativeOutput", []));
  fn "__runProcess" [] [TRecord ("Darklang.Stdlib.Cli.NativeProcessRequest", [])] (TRecord ("Darklang.Stdlib.Cli.NativeProcessOutput", []));
  fn "__hostOSCode" [] [] (TInt64);
  fn "__hostArchitectureCode" [] [] (TInt64);
  fn "__hostname" [] [] (TSum ("Darklang.Stdlib.Result.Result", [TString; TRecord ("Darklang.Stdlib.Cli.NativePosixError", [])]));
  fn "__getenv" [] [TString] (TSum ("Darklang.Stdlib.Option.Option", [TString]));
  fn "__createExclusive" [] [TString] (TInt64);
  fn "__environmentPacked" [] [] (TString);
  fn "__setenv" [] [TString; TString] (TSum ("Darklang.Stdlib.Result.Result", [TUnit; TRecord ("Darklang.Stdlib.Cli.NativePosixError", [])]));
  fn "__unsetenv" [] [TString] (TSum ("Darklang.Stdlib.Result.Result", [TUnit; TRecord ("Darklang.Stdlib.Cli.NativePosixError", [])]));
  fn "__kill" [] [TInt64; TInt64] (TSum ("Darklang.Stdlib.Result.Result", [TUnit; TRecord ("Darklang.Stdlib.Cli.NativePosixError", [])]));
  fn "__sleep" [] [TFloat64] (TUnit);
  fn "__getpid" [] [] (TInt64);
  fn "__getuid" [] [] (TInt64);
  fn "__cpuCount" [] [] (TInt64);
  fn "__spawnProcess" [] [TString] (TInt64);
  fn "__processIO" [] [TInt64; TString] (TRecord ("Darklang.Stdlib.Cli.NativeOutput", []));
  fn "__terminateProcess" [] [TInt64] (TRecord ("Darklang.Stdlib.Cli.NativeOutput", []));
]}
(*
   Compiler-only file effects used by portable stdlib implementations.
*)
let fileIntrinsicModule : AST.moduleDef = {AST.name = "Darklang.Stdlib.File"; functions = [
  fn "currentDirectory" [] [] (TString);
  fn "listDirectoryPacked" [] [TString] (TString);
  fn "readBlob" [] [TString] (resultType TBlob);
  fn "exists" [] [TString] (TBool);
  fn "isDirectory" [] [TString] (TBool);
  fn "writeBlob" [] [TString; TBlob] (resultType TUnit);
  fn "appendText" [] [TString; TString] (resultType TUnit);
  fn "delete" [] [TString] (resultType TUnit);
  fn "createDirectory" [] [TString] (resultType TUnit);
  fn "setExecutable" [] [TString] (resultType TUnit);
  fn "writeFromPtr" [] [TString; TInternalRawPtr; TInt64] (TUnit);
]}
(*
   Private socket syscalls. The Dark Network module converts signed errno
   results into typed values and owns the descriptors returned here.
*)
let networkIntrinsicModule : AST.moduleDef = {AST.name = "Darklang.Stdlib.Network"; functions = [
  fn "__tcp4Socket" [] [] (TInt64);
  fn "__close" [] [TInt64] (TInt64);
]}
(*
   Private entropy primitive used to implement the upstream numeric random APIs.
*)
let randomModule : AST.moduleDef = {AST.name = "Darklang.Stdlib.Int"; functions = [
  fn "__randomInt64Word" [] [] (TInt64);
]}
(*
   Internal typed operations used by the portable Stdlib.DateTime module.
*)
let dateTimeModule : AST.moduleDef = {AST.name = "Darklang.Stdlib.DateTime"; functions = [
  fn "__now" [] [] (TDateTime);
  fn "__fromUnixTimeTicks" [] [TInt64] (TDateTime);
  fn "__toUnixTimeTicks" [] [TDateTime] (TInt64);
]}
(*
   Explicit CLI presentation primitives. These are compiler intrinsics rather
   than host-library calls, so their effects are visible throughout the IR.
*)
let builtinPresentationModule : AST.moduleDef = {AST.name = "Builtin"; functions = [
  fn "print" [] [TString] (TUnit);
  fn "printLine" [] [TString] (TUnit);
  fn "stdinReadLine" [] [TUnit] (TString);
]}
(*
   Narrow package lookup surface consumed by ValueSearch. Compilation replaces
   these signatures with catalog-backed Dark functions for each concrete AOT
   specialization; there is no live package-manager service in native output.
*)
let packageCatalogModule : AST.moduleDef = {AST.name = "Builtin"; functions = [
  fn "pmFindValuesByValueType" [] [TSum ("Darklang.LanguageTools.RuntimeTypes.ValueType", [])] (TList (TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])));
  fn "pmGetLocationsByValue" [] [TString; TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])] (TList (TRecord ("Darklang.LanguageTools.ProgramTypes.PackageLocation", [])));
  fn "pmEvaluateValue" ["a"] [TSum ("Darklang.LanguageTools.ProgramTypes.Hash", [])] (TSum ("Darklang.Stdlib.Option.Option", [TVar "a"]));
]}
(*
   Raw memory intrinsics - internal only for HAMT implementation
   These functions bypass the type system and should only be used in stdlib code
   The names start with __ to indicate they are internal
   Mapped buffers have a private length prefix and must be explicitly unmapped.
   Fixed RC layout of the runtime list-array allocator's small branch.
   __raw_alloc : (Int64) -> RawPtr - allocate raw bytes
   __raw_free : (RawPtr) -> Unit - free an internal 8-byte raw cell
   __raw_get<v> : (RawPtr, Int64) -> v - read 8 bytes at offset, typed as v
   __raw_take<v> transfers a typed slot edge to the caller before it is cleared.
   __raw_write_word : (RawPtr, Int64, Int64) -> Unit - write 8 unmanaged bytes at offset
   __raw_get_byte : (RawPtr, Int64) -> Int64 - read 1 byte at offset, zero-extended
   __raw_write_byte : (RawPtr, Int64, Int64) -> Unit - write 1 unmanaged byte at offset
   __raw_slot_init<v> : (RawPtr, Int64, v) -> Unit - initialize a typed slot edge
   __refcount_inc_string : (String) -> Unit - increment string refcount
   __refcount_dec_string : (String) -> Unit - decrement string refcount, free if 0
   __string_to_rawptr : (String) -> RawPtr - borrow string backing pointer
   __rawptr_to_string : (RawPtr) -> String - reinterpret initialized raw allocation as String
   __string_concat_raw : (String, String) -> String - internal byte concat;
   public concatenation normalizes the result to NFC.
   Int uses a tagged machine word: odd words are signed small integers and
   aligned words point at immutable limb buffers. These representation
   views are private to the bigint implementation.
   Int128 and UInt128 are immutable fixed blocks containing low/high UInt64 limbs.
   These representation views are ownership-neutral; the RawPtr-to-value views
   adopt a fully initialized owned allocation.
   Blob intrinsics - for byte array operations
   __blob_to_rawptr : (Blob) -> RawPtr - borrow bytes backing pointer
   __rawptr_to_blob : (RawPtr) -> Blob - reinterpret initialized raw allocation as Blob
   Stream handles are opaque source values with a raw-pointer runtime representation.
   Dict intrinsics - for type-safe Dict<k, v> operations
   __empty_dict<k, v> : () -> Dict<k, v> - create empty dict (null pointer)
   __dict_is_null<k, v> : (Dict<k, v>) -> Bool - check if dict is empty/null
   __dict_get_tag<k, v> : (Dict<k, v>) -> Int64 - get tag bits from dict pointer
   __dict_to_rawptr<k, v> : (Dict<k, v>) -> RawPtr - convert dict to raw pointer (strips tag)
   __rawptr_to_dict<k, v> : (RawPtr, Int64) -> Dict<k, v> - create dict from pointer + tag
   Key intrinsics - for generic key hashing and comparison
   __hash<k> : (k) -> Int64 - hash any key type
   __key_eq<k> : (k, k) -> Bool - compare two keys for equality
   __compare<a> is an AOT-only dispatch marker. Type checking replaces every
   concrete use with a synthesized canonical three-way comparison helper.
   List intrinsics for the direct-payload skew RAL implementation.
   __list_empty<a> : () -> List<a> - create empty list (null pointer with tag 0)
   __list_is_null<a> : (List<a>) -> Bool - check if list is empty/null
   __list_get_tag<a> : (List<a>) -> Int64 - get tag bits from list pointer (low 3 bits)
   __list_to_rawptr<a> : (List<a>) -> RawPtr - convert list to raw pointer (strips tag)
   __rawptr_to_list<a> : (RawPtr, Int64) -> List<a> - create list from pointer + tag
*)
let rawMemoryIntrinsics = [
  fn "__mapped_alloc" [] [TInt64] (TInternalRawPtr);
  fn "__mapped_free" [] [TInternalRawPtr] (TUnit);
  fn "__list_array_release_small" [] [TInternalRawPtr] (TUnit);
  fn "__raw_alloc" [] [TInt64] (TInternalRawPtr);
  fn "__raw_free" [] [TInternalRawPtr] (TUnit);
  fn "__raw_get" ["v"] [TInternalRawPtr; TInt64] (TVar "v");
  fn "__raw_take" ["v"] [TInternalRawPtr; TInt64] (TVar "v");
  fn "__raw_write_word" [] [TInternalRawPtr; TInt64; TInt64] (TUnit);
  fn "__raw_get_byte" [] [TInternalRawPtr; TInt64] (TInt64);
  fn "__raw_write_byte" [] [TInternalRawPtr; TInt64; TInt64] (TUnit);
  fn "__raw_slot_init" ["v"] [TInternalRawPtr; TInt64; TVar "v"] (TUnit);
  fn "__refcount_inc_string" [] [TString] (TUnit);
  fn "__refcount_dec_string" [] [TString] (TUnit);
  fn "__string_to_rawptr" [] [TString] (TInternalRawPtr);
  fn "__rawptr_to_string" [] [TInternalRawPtr] (TString);
  fn "__string_concat_raw" [] [TString; TString] (TString);
  fn "__int_to_word" [] [TInt] (TInt64);
  fn "__word_to_int" [] [TInt64] (TInt);
  fn "__int_to_rawptr" [] [TInt] (TInternalRawPtr);
  fn "__rawptr_to_int" [] [TInternalRawPtr] (TInt);
  fn "__int64_to_uint64_bits" [] [TInt64] (TUInt64);
  fn "__uint64_to_int64_bits" [] [TUInt64] (TInt64);
  fn "__uint8_to_int64" [] [TUInt8] (TInt64);
  fn "__uint16_to_int64" [] [TUInt16] (TInt64);
  fn "__uint32_to_int64" [] [TUInt32] (TInt64);
  fn "__int128_to_rawptr" [] [TInt128] (TInternalRawPtr);
  fn "__rawptr_to_int128" [] [TInternalRawPtr] (TInt128);
  fn "__uint128_to_rawptr" [] [TUInt128] (TInternalRawPtr);
  fn "__rawptr_to_uint128" [] [TInternalRawPtr] (TUInt128);
  fn "__int128_to_int" [] [TInt128] (TInt);
  fn "__uint128_to_int" [] [TUInt128] (TInt);
  fn "__int_to_int128" [] [TInt] (TInt128);
  fn "__int_to_uint128" [] [TInt] (TUInt128);
  fn "__int64_to_int8" [] [TInt64] (TInt8);
  fn "__int64_to_int16" [] [TInt64] (TInt16);
  fn "__int64_to_int32" [] [TInt64] (TInt32);
  fn "__int64_to_uint8" [] [TInt64] (TUInt8);
  fn "__int64_to_uint16" [] [TInt64] (TUInt16);
  fn "__int64_to_uint32" [] [TInt64] (TUInt32);
  fn "__blob_to_rawptr" [] [TBlob] (TInternalRawPtr);
  fn "__rawptr_to_blob" [] [TInternalRawPtr] (TBlob);
  fn "__stream_to_rawptr" ["a"] [TStream(TVar "a")] (TInternalRawPtr);
  fn "__rawptr_to_stream" ["a"] [TInternalRawPtr] (TStream(TVar "a"));
  fn "__empty_dict" ["k"; "v"] [] (TDict(TVar "k", TVar "v"));
  fn "__dict_is_null" ["k"; "v"] [TDict(TVar "k", TVar "v")] (TBool);
  fn "__dict_get_tag" ["k"; "v"] [TDict(TVar "k", TVar "v")] (TInt64);
  fn "__dict_to_rawptr" ["k"; "v"] [TDict(TVar "k", TVar "v")] (TInternalRawPtr);
  fn "__rawptr_to_dict" ["k"; "v"] [TInternalRawPtr; TInt64] (TDict(TVar "k", TVar "v"));
  fn "__hash" ["k"] [TVar "k"] (TInt64);
  fn "__key_eq" ["k"] [TVar "k"; TVar "k"] (TBool);
  fn "__compare" ["a"] [TVar "a"; TVar "a"] (TInt64);
  fn "__list_empty" ["a"] [] (TList(TVar "a"));
  fn "__list_is_null" ["a"] [TList(TVar "a")] (TBool);
  fn "__list_get_tag" ["a"] [TList(TVar "a")] (TInt64);
  fn "__list_to_rawptr" ["a"] [TList(TVar "a")] (TInternalRawPtr);
  fn "__rawptr_to_list" ["a"] [TInternalRawPtr; TInt64] (TList(TVar "a"));
]
(*
   All intrinsic Stdlib modules
*)
let allModules = [
 boolIntrinsicModule;
 integerBitwiseModule "Int8" TInt8; integerBitwiseModule "Int16" TInt16; integerBitwiseModule "Int32" TInt32;
 int64IntrinsicModule;
 integerBitwiseModule "UInt8" TUInt8; integerBitwiseModule "UInt16" TUInt16; integerBitwiseModule "UInt32" TUInt32; integerBitwiseModule "UInt64" TUInt64;
 floatIntrinsicModule; cliIntrinsicModule; fileIntrinsicModule; networkIntrinsicModule; randomModule; dateTimeModule; builtinPresentationModule; packageCatalogModule
]
(*
   Build the module registry from all modules
   Maps qualified function names (e.g., "Darklang.Stdlib.Int64.add") to their definitions
   Add raw memory intrinsics directly (no module prefix)
*)
let buildModuleRegistry () =
 let moduleFuncs = List.concat_map (fun (definition : AST.moduleDef) -> List.map (fun (func : AST.moduleFunc) -> definition.name ^ "." ^ func.name, func) definition.functions) allModules in
 let raw = List.map (fun (func : AST.moduleFunc) -> func.name, func) rawMemoryIntrinsics in
 M.of_list (moduleFuncs @ raw)
(*
   Get a function by the exact identity attached during type checking.
*)
let tryGetFunction registry name = Option.map (fun func -> func, name) (M.find_opt name registry)
(*
   Get the type of a module function as an AST.SemanticType
*)
let getFunctionType (func : AST.moduleFunc) = TFunction (func.paramTypes, func.returnType)
