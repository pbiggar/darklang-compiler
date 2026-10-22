(* source_selection_regression.ml - Execute replacements and reject changed public contracts. *)
open Dark_compiler
module C = CompilationContexts
module O = CompilerOptions
module Selection = SourceSelection

let require = function Ok result -> result | Error message -> failwith message
let assert_ condition message = if not condition then failwith message
let reasons = [ "interpreter-intrinsic"; "runtime-reflection" ]
let source text = require (SourceInventory.parse ~reasons text)

let select original replacement =
  Selection.select ~context:SourceContracts.emptyContext
    ~roots:[ "upstream.dark" ] ~supplements:[]
    ~sources:
      [
        ("upstream.dark", source original); ("compiler.dark", source replacement);
      ]

let error text = function
  | Error message ->
      assert_ (Text.contains message text) ("Unexpected error: " ^ message)
  | Ok _ -> failwith ("Expected failure: " ^ text)

let original =
  {|module Example
type Count = Int64
// @compiler replace: compiler.dark#Box reason=interpreter-intrinsic
type Box<'a> = { value: 'a }
// @compiler replace: compiler.dark#seed reason=interpreter-intrinsic
val seed = 1L
// @compiler replace: compiler.dark#decode reason=interpreter-intrinsic
let decode (input: Count) : Int64 = Missing.interpreterIntrinsic input
// @compiler excluded: runtime-reflection
let reflect () : Int64 = Missing.reflection ()
test retained = Missing.testBody ()
|}

let replacement =
  {|module Example
type Box<'b> = { value: 'b }
val seed = 2L
let __increment (input: Int64) : Int64 = input + seed
let decode (input: Int64) : Int64 = __increment input
|}

let () =
  let supplemented =
    require
      (Selection.select ~context:SourceContracts.emptyContext
         ~roots:[ "upstream.dark" ] ~supplements:[ "compiler.dark" ]
         ~sources:
           [
             ("upstream.dark", source original);
             ( "compiler.dark",
               source
                 (replacement ^ "\nlet compilerOnly () : Int64 = decode seed")
             );
           ])
  in
  let useSupplement =
    require (WrittenParsing.parse Validation.Script "Example.compilerOnly ()")
  in
  ignore
    (require
       (WrittenChecking.checkSourceUnits true false
          (List.map snd supplemented.Selection.units @ [ useSupplement ])));
  let selected = require (select original replacement) in
  error "runtime-reflection" (Selection.require selected "Example.reflect");
  require (Selection.require selected "Example.decode");
  let units = List.map snd selected.Selection.units in
  ignore (require (WrittenChecking.checkSourceUnits true false units));
  let generic body =
    "module Example\n\
     // @compiler replace: compiler.dark#identity reason=interpreter-intrinsic\n\
     let identity<'a> (value: 'a) : 'a = " ^ body
  in
  ignore
    (require
       (select
          (generic "Missing.intrinsic value")
          "module Example\nlet identity<'b> (value: 'b) : 'b = value"));
  error "contract mismatch"
    (select
       (generic "Missing.intrinsic value")
       "module Example\nlet identity (value: Int64) : Int64 = value");
  error "contract mismatch"
    (select
       "module Example\n\
        // @compiler replace: compiler.dark#f reason=interpreter-intrinsic\n\
        let f (x: Int64) :{Clock} Int64 = 1L"
       "module Example\nlet f (x: Int64) :{} Int64 = x");
  error "contract mismatch"
    (select
       "module Example\n\
        // @compiler replace: compiler.dark#Box reason=interpreter-intrinsic\n\
        type Box = { value: Int64 }"
       "module Example\ntype Box = { renamed: Int64 }");
  error "contract mismatch"
    (select
       "module Example\n\
        // @compiler replace: compiler.dark#seed reason=interpreter-intrinsic\n\
        val seed = 1L"
       "module Example\nval seed = true");
  error "Cannot establish upstream val contract"
    (select
       "module Example\n\
        // @compiler replace: compiler.dark#seed reason=interpreter-intrinsic\n\
        val seed = Missing.intrinsic ()"
       "module Example\nval seed = true");
  error "Missing replacement"
    (select
       "module Example\n\
        // @compiler replace: compiler.dark#missing reason=interpreter-intrinsic\n\
        let decode () : Int64 = 1L"
       replacement);
  error "preserve public identity"
    (select
       "module Other\n\
        // @compiler replace: compiler.dark#decode reason=interpreter-intrinsic\n\
        let decode (x: Int64) : Int64 = 1L"
       replacement);
  error "concrete unannotated"
    (select
       "module Example\n\
        // @compiler replace: compiler.dark#decode reason=interpreter-intrinsic\n\
        let decode (x: Int64) : Int64 = Missing.original x"
       "module Example\n\
        // @compiler replace: upstream.dark#decode reason=interpreter-intrinsic\n\
        let decode (x: Int64) : Int64 = x");
  let blocked =
    require
      (select
         "module Example\n\
          trait Inspect<'a> =\n\
         \  // @compiler excluded: runtime-reflection\n\
         \  let inspect (value: 'a) : String\n\
          let ordinary () : Int64 = 1L"
         "")
  in
  error "runtime-reflection" (Selection.require blocked "Example.Inspect");
  ignore
    (require
       (WrittenChecking.checkSourceUnits true false
          (List.map snd blocked.Selection.units)));
  let target = require (Platform.detectHostTarget ()) in
  let stdlib = require (StdlibCompilation.buildStdlib target) in
  let entry =
    require
      (WrittenParsing.parse Validation.Script
         "if Example.decode 5L == 7L then 42L else Builtin.crash \"Wrong \
          replacement\"")
  in
  let entryTree = Validation.ValidatedSourceFile.toWrittenTypes entry in
  let trees =
    List.map Validation.ValidatedSourceFile.toWrittenTypes units @ [ entryTree ]
  in
  let sources =
    List.mapi
      (fun index _ ->
        {
          C.name = "replacement-" ^ string_of_int index;
          purpose =
            (if index = List.length trees - 1 then
               NameSyntax.SourceUnitPurpose.Executable
             else NameSyntax.SourceUnitPurpose.Library);
          source = "";
        })
      trees
  in
  let request : C.compileRequest =
    {
      C.context = C.StdlibOnly stdlib;
      mode = O.TestExpression;
      sources = NonEmptyList.fromList sources;
      allowInternal = true;
      verbosity = 0;
      options = O.defaultOptions;
      packageValues = C.emptyPackageValueCatalog;
      packageManager = None;
      passTimingRecorder = None;
      session = None;
    }
  in
  let report = CompilerLibrary.compileWritten request trees in
  let binary = require report.O.result in
  let output = CompilerExecution.executeCaptured target 0 O.Closed binary in
  assert_
    (output.O.exitCode = 0 && String.trim output.O.stdout = "42")
    ("Selected replacement did not execute: " ^ output.O.stdout
   ^ output.O.stderr);
  print_endline
    "Source replacements preserve contracts, skip unavailable bodies and \
     execute natively"
