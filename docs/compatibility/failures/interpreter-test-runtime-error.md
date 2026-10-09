# Unsupported interpreter runtime-error assertions

`Builtin.testRuntimeError` is interpreter test infrastructure and is deliberately
unsupported by the compiler. Compiler-owned programs use `Builtin.crash`.
These imported assertions remain unchanged and are excluded individually.

The exclusions cover **69 assertions in 19 files**. Four were already excluded
for other diagnostics; retiring this builtin adds 65 exclusions. The imported
application fixture uses it through the shared `derrorFn` helper at L48; that
ungated fixture run failed all 34 during preamble compilation. Disabling only
L118 lets the existing fallback prune that helper, and all other 33 assertions
pass and remain enabled.

| File | Individual tests |
| --- | --- |
| [language/apply/eapply.dark](../../../test/fixtures/e2e/upstream/language/apply/eapply.dark) | [L118](../../../test/fixtures/e2e/upstream/language/apply/eapply.dark#L118) |
| [language/apply/einfix.dark](../../../test/fixtures/e2e/upstream/language/apply/einfix.dark) | [L57](../../../test/fixtures/e2e/upstream/language/apply/einfix.dark#L57), [L58](../../../test/fixtures/e2e/upstream/language/apply/einfix.dark#L58), [L59](../../../test/fixtures/e2e/upstream/language/apply/einfix.dark#L59) |
| [language/basic/eand.dark](../../../test/fixtures/e2e/upstream/language/basic/eand.dark) | [L5](../../../test/fixtures/e2e/upstream/language/basic/eand.dark#L5), [L13](../../../test/fixtures/e2e/upstream/language/basic/eand.dark#L13) |
| [language/basic/elet.dark](../../../test/fixtures/e2e/upstream/language/basic/elet.dark) | [L1](../../../test/fixtures/e2e/upstream/language/basic/elet.dark#L1), [L2](../../../test/fixtures/e2e/upstream/language/basic/elet.dark#L2), [L19](../../../test/fixtures/e2e/upstream/language/basic/elet.dark#L19), [L20](../../../test/fixtures/e2e/upstream/language/basic/elet.dark#L20), [L45](../../../test/fixtures/e2e/upstream/language/basic/elet.dark#L45), [L54](../../../test/fixtures/e2e/upstream/language/basic/elet.dark#L54), [L60](../../../test/fixtures/e2e/upstream/language/basic/elet.dark#L60), [L66](../../../test/fixtures/e2e/upstream/language/basic/elet.dark#L66), [L71](../../../test/fixtures/e2e/upstream/language/basic/elet.dark#L71), [L82](../../../test/fixtures/e2e/upstream/language/basic/elet.dark#L82) |
| [language/basic/eor.dark](../../../test/fixtures/e2e/upstream/language/basic/eor.dark) | [L18](../../../test/fixtures/e2e/upstream/language/basic/eor.dark#L18), [L19](../../../test/fixtures/e2e/upstream/language/basic/eor.dark#L19) |
| [language/collections/dlist.dark](../../../test/fixtures/e2e/upstream/language/collections/dlist.dark) | [L9](../../../test/fixtures/e2e/upstream/language/collections/dlist.dark#L9), [L10](../../../test/fixtures/e2e/upstream/language/collections/dlist.dark#L10) |
| [language/collections/dtuple.dark](../../../test/fixtures/e2e/upstream/language/collections/dtuple.dark) | [L9](../../../test/fixtures/e2e/upstream/language/collections/dtuple.dark#L9), [L11](../../../test/fixtures/e2e/upstream/language/collections/dtuple.dark#L11) |
| [language/custom-data/enums.dark](../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark) | [L7](../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L7), [L9](../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L9), [L11](../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L11), [L72](../../../test/fixtures/e2e/upstream/language/custom-data/enums.dark#L72) |
| [language/custom-data/record-field-acess.dark](../../../test/fixtures/e2e/upstream/language/custom-data/record-field-acess.dark) | [L13](../../../test/fixtures/e2e/upstream/language/custom-data/record-field-acess.dark#L13) |
| [language/custom-data/records.dark](../../../test/fixtures/e2e/upstream/language/custom-data/records.dark) | [L8](../../../test/fixtures/e2e/upstream/language/custom-data/records.dark#L8) |
| [language/derror.dark](../../../test/fixtures/e2e/upstream/language/derror.dark) | [L13](../../../test/fixtures/e2e/upstream/language/derror.dark#L13), [L14](../../../test/fixtures/e2e/upstream/language/derror.dark#L14), [L15](../../../test/fixtures/e2e/upstream/language/derror.dark#L15), [L16](../../../test/fixtures/e2e/upstream/language/derror.dark#L16), [L17](../../../test/fixtures/e2e/upstream/language/derror.dark#L17), [L18](../../../test/fixtures/e2e/upstream/language/derror.dark#L18), [L19](../../../test/fixtures/e2e/upstream/language/derror.dark#L19), [L20](../../../test/fixtures/e2e/upstream/language/derror.dark#L20), [L21](../../../test/fixtures/e2e/upstream/language/derror.dark#L21), [L22](../../../test/fixtures/e2e/upstream/language/derror.dark#L22), [L23](../../../test/fixtures/e2e/upstream/language/derror.dark#L23), [L25](../../../test/fixtures/e2e/upstream/language/derror.dark#L25), [L32](../../../test/fixtures/e2e/upstream/language/derror.dark#L32) |
| [language/elambda.dark](../../../test/fixtures/e2e/upstream/language/elambda.dark) | [L13](../../../test/fixtures/e2e/upstream/language/elambda.dark#L13), [L18](../../../test/fixtures/e2e/upstream/language/elambda.dark#L18), [L21](../../../test/fixtures/e2e/upstream/language/elambda.dark#L21), [L33](../../../test/fixtures/e2e/upstream/language/elambda.dark#L33), [L35](../../../test/fixtures/e2e/upstream/language/elambda.dark#L35) |
| [language/error-syntax.dark](../../../test/fixtures/e2e/upstream/language/error-syntax.dark) | [L1](../../../test/fixtures/e2e/upstream/language/error-syntax.dark#L1) |
| [language/flow-control/eif.dark](../../../test/fixtures/e2e/upstream/language/flow-control/eif.dark) | [L3](../../../test/fixtures/e2e/upstream/language/flow-control/eif.dark#L3), [L4](../../../test/fixtures/e2e/upstream/language/flow-control/eif.dark#L4), [L5](../../../test/fixtures/e2e/upstream/language/flow-control/eif.dark#L5), [L6](../../../test/fixtures/e2e/upstream/language/flow-control/eif.dark#L6), [L10](../../../test/fixtures/e2e/upstream/language/flow-control/eif.dark#L10), [L23](../../../test/fixtures/e2e/upstream/language/flow-control/eif.dark#L23), [L26](../../../test/fixtures/e2e/upstream/language/flow-control/eif.dark#L26) |
| [language/flow-control/ematch.dark](../../../test/fixtures/e2e/upstream/language/flow-control/ematch.dark) | [L656](../../../test/fixtures/e2e/upstream/language/flow-control/ematch.dark#L656), [L660](../../../test/fixtures/e2e/upstream/language/flow-control/ematch.dark#L660), [L663](../../../test/fixtures/e2e/upstream/language/flow-control/ematch.dark#L663), [L667](../../../test/fixtures/e2e/upstream/language/flow-control/ematch.dark#L667), [L672](../../../test/fixtures/e2e/upstream/language/flow-control/ematch.dark#L672), [L677](../../../test/fixtures/e2e/upstream/language/flow-control/ematch.dark#L677), [L681](../../../test/fixtures/e2e/upstream/language/flow-control/ematch.dark#L681) |
| [language/flow-control/epipe.dark](../../../test/fixtures/e2e/upstream/language/flow-control/epipe.dark) | [L11](../../../test/fixtures/e2e/upstream/language/flow-control/epipe.dark#L11) |
| [stdlib/dict.dark](../../../test/fixtures/e2e/upstream/stdlib/dict.dark) | [L76](../../../test/fixtures/e2e/upstream/stdlib/dict.dark#L76), [L93](../../../test/fixtures/e2e/upstream/stdlib/dict.dark#L93) |
| [stdlib/list.dark](../../../test/fixtures/e2e/upstream/stdlib/list.dark) | [L157](../../../test/fixtures/e2e/upstream/stdlib/list.dark#L157), [L184](../../../test/fixtures/e2e/upstream/stdlib/list.dark#L184), [L238](../../../test/fixtures/e2e/upstream/stdlib/list.dark#L238), [L346](../../../test/fixtures/e2e/upstream/stdlib/list.dark#L346) |
| [stdlib/nomodule.dark](../../../test/fixtures/e2e/upstream/stdlib/nomodule.dark) | [L308](../../../test/fixtures/e2e/upstream/stdlib/nomodule.dark#L308) |

Expected compiler boundary: `Unknown function or value 'Builtin.testRuntimeError'`.
This is an unsupported interpreter facility, rather than a request to add its
semantics to the compiler. Runtime failure behavior is covered by
[`crash-builtin.e2e`](../../../test/fixtures/e2e/crash-builtin.e2e).

An individual ungated rerun of all 19 files executed 1,116 assertions:
**1,006 passed and 110 failed**. The failures comprised 69 infrastructure
assertions listed above, 33 cascading preamble failures, and eight existing exclusions
(three constructor-error assertions and five counter-instrumentation assertions). `derror.dark:L10` still
passes its expected unknown-function compile error and remains enabled.
