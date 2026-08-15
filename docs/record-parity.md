# Record parity evidence

This implementation was revalidated against these exact revisions:

- comparison compiler: `b2e1f3d1e4ce0338d4c4662db9a1326f2e2cb899`;
- implementation-base compiler HEAD: `a78567efd773de86265e55a54445ddf5a5a8911c`;
- interpreter: `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`;
- DCB1 inventory starting point: report commit `8a402797`.

DCB1 was used only to locate candidates. Every retained finding was checked
again. The interpreter's public behavior is the baseline, and performance-only
differences are excluded.

| Contract | Compiler evidence | Pinned interpreter evidence | Result |
| --- | --- | --- | --- |
| Grammar | both `passes/1_Parser.fs` entry paths | `LibParser/Parser.fs:1634-1644,1945-1970,2063-2077,2245-2399,2760-2805` | Named/qualified/generic construction, mixed separators, non-empty declarations/updates; anonymous literals rejected |
| Expression identity | `AST.fs` formerly stored only a string and dropped literal arguments | `LibExecution/ProgramTypes.fs:479-493` | Explicit source name, resolved name, and ordered arguments |
| Declaration metadata | `passes/1.5_TypeChecking.fs` formerly inferred parameters and used a last-wins map | `ProgramTypes.fs:594-610`; `RuntimeTypes.fs:1503-1513` | Declared/phantom parameters and order retained; first-declared lookup |
| Construction | type-checker record literal and `passes/2_AST_to_ANF.fs` | `LibTypeChecker/TypeChecker.fs:1093-1246`; `ProgramTypesToRuntimeTypes.fs:1077-1102` | Duplicate/missing/unknown/empty/type validation and source-order evaluation |
| Access/update | type-checker access/update and ANF projection/clone | `Interpreter.fs:1985-2002,2260-2288`; pinned fixtures | Same behavior, with earlier AOT diagnostics |
| Patterns | former `PRecord` parser/type/lowering sites | pinned program/runtime pattern unions; CLI record docs | Compiler extension removed; bind the record then access fields |
| Runtime identity | records formerly used only `TupleAlloc`/`TupleGet` | `RuntimeTypes.fs:790-804,984-990`; `Interpreter.fs:2236-2288` | Distinct descriptor slot and record-specific ANF operations |
| Ownership | `ANF.fs`, `passes/2.5_RefCountInsertion.fs`, both backends | runtime `DRecord` owns its field values | Descriptor is immediate; concrete fields retain/release recursively |
| Equality | helpers formerly used tuple projections | `NoModule.fs:80-94` | Nominal compatibility AOT; aliases resolve together; recursive named fields |
| Rendering | `passes/1.6_ValueRendering.fs` formerly inferred args and forced one line | `prettyPrinter/runtimeTypes.dark:500-525` | Resolved name/args, ordinal keys, recursive values, 80-character layout |

Focused same-source evidence remains unchanged in
`src/Tests/e2e/upstream/language/custom-data/records.dark` and
`record-field-acess.dark`. Their supported success, evaluation-order, and
update lines are selected directly by the test runner; compiler regressions in
`records.e2e` and `syntax/records.syntax` make AOT diagnostic phases explicit.

## Surviving compiler-only behavior

- Statically invalid operations fail during parsing or type checking.
- Generic records are monomorphized ahead of time; there is no runtime type
  inference.
- Native code stores a deterministic descriptor identity word instead of the
  interpreter's F# runtime object. This preserves observable nominal identity.
